:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(date_time)).
:- use_module(library(url)).

calendarId('7p6iutdbluv6bqbssbulkv0hug@group.calendar.google.com').
calendarApiKey('AIzaSyDoLPGkqwhBEAyceyIS5XPVahFm4Gq81-A').

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

listEventsURL(TimeMin, TimeMax, Url) :-
  calendarId(Id),
  calendarApiKey(Key),
  format(string(Decoded), 'https://www.googleapis.com/calendar/v3/calendars/~s/events?key=~s&timeMin=~s&timeMax=~s', [Id, Key, TimeMin, TimeMax]),
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
  L = EventsData.get('items'),
  maplist(jsonToList, L, R).

insertEventURL(Url) :-
  calendarId(Id),
  calendarApiKey(Key),
  format(string(Decoded), 'https://www.googleapis.com/calendar/v3/calendars/~s/events?key=~s', [Id, Key]),
  url_iri(Url, Decoded). % encode url

insertEventCall(Title, Start, End, R) :-
  Body = json{
    summary: Title,
    end: json{
      dateTime: End
    },
    start: json{
      dateTime: Start
    }
  },
  insertEventURL(Url),
  setup_call_cleanup(
      http_open(
          Url, 
          In, 
        [
          post(json(Body)),
          status_code(Code),
          request_header('Content-Type'='application/json'),
          request_header('Accept'='application/json'),
        ]
      ),
      json_read_dict(In, InsertData),
      close(In)
  ),
  write(Code).
