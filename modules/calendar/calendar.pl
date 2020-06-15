:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(date_time)).
:- use_module(library(url)).
:- use_module(library(http/http_server)).

% :- module(calendar, [
%     calendarApiUrl/1,
%     calendarId/1,
%     strSign/2,
%     utcOffsetToString/2,
%     rfc339Timestamp/2,
%     jsonToList/2,
%     listEventsURL/3,
%     listEventsCall/2,
%     insertEventCall/5,
%     updateEventURL/2,
%     updateEventCall/3
%   ]).

calendarApiUrl('http://127.0.0.1:5001/api/calendar/').
calendarId('7p6iutdbluv6bqbssbulkv0hug@group.calendar.google.com').

% Returns the string representation of the sign of a number.
strSign(Nbr, "-") :- number(Nbr), Nbr < 0, !.
strSign(Nbr, "+") :- number(Nbr).

% Given an UTC offset as seconds return the human representation of it.
utcOffsetToString(Offset, R) :-
  number(Offset),
  strSign(Offset, S),
  Offset_ is abs(Offset),
  Hours   is div(Offset_, 3600),
  Minutes is mod(Offset_, 3600),
  twoDigits(Hours,   H),
  twoDigits(Minutes, M),
  format(string(R), '~s~s:~s', [S, H, M]).

% Zero pad number to two digits. Returns a string representation.
twoDigits(Nbr, R) :-
  format(string(R), '~|~`0t~w~2|', [Nbr]).

get_date(Date) :-
  get_time(T),
  stamp_date_time(T, Date, local).

printEvents([]).
printEvents([Event | T]) :-
  Start = Event.get('start').get('dateTime'),
  End   = Event.get('end').get('dateTime'),
  Title = Event.get('summary'),
  format('Evenimentul se nume\u0219te ~s. \u00eentre ~s ~s~n', [Title, Start, End]),
  printEvents(T).

% The calendar api uses rfc-3339 for formatting timestamps.
rfc339Timestamp(Date, Timestamp) :-
  date_time_value(year,       Date, Year),
  date_time_value(month,      Date, Month),
  date_time_value(day,        Date, Day),
  date_time_value(hour,       Date, Hour),
  date_time_value(minute,     Date, Minute),
  date_time_value(second,     Date, Second),
  date_time_value(utc_offset, Date, Offset),

  Second_ is floor(Second),
  Offset_ is Offset * -1,
  utcOffsetToString(Offset_, O),

  twoDigits(Month,   M),
  twoDigits(Day,     D),
  twoDigits(Hour,    H),
  twoDigits(Minute,  Min),
  twoDigits(Second_, S),

  format(string(Timestamp), '~d-~s-~sT~s:~s:~s~s', [Year, M, D, H, Min, S, O]).

rfc339Timestamp(datetime(Year, Month, Day, Hour, Minute, Second), Timestamp) :-
  Offset is -10800,
  Second_ is floor(Second),
  Offset_ is Offset * -1,
  utcOffsetToString(Offset_, O),

  twoDigits(Month,   M),
  twoDigits(Day,     D),
  twoDigits(Hour,    H),
  twoDigits(Minute,  Min),
  twoDigits(Second_, S),

  format(string(Timestamp), '~d-~s-~sT~s:~s:~s~s', [Year, M, D, H, Min, S, O]).


listEventsURL(TimeMin, TimeMax, Url) :-
  calendarApiUrl(ApiUrl),
  format(string(Decoded), '~s?start=~s&end=~s', [ApiUrl, TimeMin, TimeMax]),
  url_iri(Url, Decoded). % encode url

% Just for display purposes. Transforms json to list of predicated representation.
jsonToList(Json, R) :-
  Start = Json.get('start').get('dateTime'),
  End   = Json.get('end').get('dateTime'),
  Title = Json.get('summary'),
  Id    = Json.get('id'),
  R = [title(Title), start(Start), end(End), id(Id)].

listEventsCall(TimeMin, TimeMax, R) :-
  listEventsURL(TimeMin, TimeMax, Url),
  setup_call_cleanup(
      http_open(Url, In, [request_header('Accept'='application/json')]),
      json_read_dict(In, EventsData),
      close(In)
  ),
  R = EventsData.get('items'),
  printEvents(R).
  % maplist(jsonToList, L, R).

insertEventCall(Title, Location, Start, End, R) :-
  Body = json([
    summary = Title,
    location = Location,
    end = End,
    start = Start
  ]),
  calendarApiUrl(Url),
  write(json(Body)),
  setup_call_cleanup(
      http_open(
          Url, 
          In, 
        [
          post(json(Body)),
          status_code(Code),
          request_header('Content-Type'='application/json'),
          request_header('Accept'='application/json')
        ]
      ),
      json_read_dict(In, InsertData),
      close(In)
  ),
  write(Code),
  jsonToList(InsertData, R).

updateEventURL(EventId, Url) :-
  calendarApiUrl(ApiUrl),
  format(string(Decoded), '~s~s/?id=~s', [ApiUrl, 'update', EventId]),
  url_iri(Url, Decoded). % encode url

% ToBeChanged: list of [key = new_value]. !! spaces are needed
updateEventCall(EventId, ToBeChanged, R) :-
  Body = json(ToBeChanged),
  updateEventURL(EventId, Url),
  write(json(Body)),
  write(Url),
  setup_call_cleanup(
      http_open(
          Url, 
          In, 
        [
          post(json(Body)),
          status_code(Code),
          request_header('Content-Type'='application/json'),
          request_header('Accept'='application/json')
        ]
      ),
      json_read_dict(In, UpdateData),
      close(In)
  ),
  write(Code),
  jsonToList(UpdateData, R).