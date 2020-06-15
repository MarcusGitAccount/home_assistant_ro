
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(date_time)).

:- consult('../calendar/calendar.pl').
:- consult('../weather/weather.pl').
:- consult('kb.pl').

:- dynamic currentState/1.
:- dynamic entity/3.
:- dynamic intent/1.

:- dynamic missingEntity/2.
:- dynamic message/1.
:- dynamic toBeModified/2.

:- http_handler('/intent', intentEndpointHandler, [methods([post])]).

server(Port) :- http_server(http_dispatch, [port(Port)]).

% Possible intents:
% :- intent(answer) %  answer received from the user of type yes/no
% :- intent(calendarUpdate) %  modify previous event calendar
% :- intent(queryWeather)
% :- intent(calendarAdd)
% :- intent(calendarAsk)

validIntent(queryWeather).
validIntent(calendarAdd).
validIntent(calendarAsk).
validIntent(calendarUpdate).
validIntent(answer).

taskIntent(queryWeather).
taskIntent(calendarAdd).
taskIntent(calendarAsk).

currentState(idle).

performState(idle) :- 
  currentState(idle), 
  intent(X),
  taskIntent(X),
  switchState(X),
  !.
% intent is not a task
performState(idle) :-
  currentState(idle).

performState(queryWeather) :-
  currentState(queryWeather),
  Entities = [loc, timp],
  getEntitiesValues(queryWeather, Entities, true),
  switchState(weatherApiCall).

performState(weatherApiCall) :-
  currentState(weatherApiCall),

  finalEntity(queryWeather, loc, LatLon, Location),
  finalEntity(queryWeather, timp, Time, T),

  % term_string(LatLon, L_),
  % term_string(Time, T_),

  log('Weather api call'),
  getWeatherCall(LatLon, Time, Desc),
  format(string(Message), 'Vremea ~s ~s. ~s.', [T, Location, Desc]),
  assertz(message(Message)),
  switchState(respond).

performState(calendarAsk) :-
  currentState(calendarAsk),
  Entities = [data, ora_inceput_relativ, ora_inceput, ora_final],
  getEntitiesValues(calendarAsk, Entities, true),
  switchState(listEvents).

performState(listEvents) :-
  currentState(listEvents),

  missingEntity(calendarAsk, ora_inceput_relativ),
  finalEntity(calendarAsk, data, D, M2),
  finalEntity(calendarAsk, ora_inceput, SL, M3),
  finalEntity(calendarAsk, ora_final, EL, M4),

  format(string(Message), 'Listare evenimente ~s \u00eentre ~s \u0219i ~s.', [M2, M3, M4]),
  assertz(message(Message)),

  log('Insert calendar API call'),

  nth0(0, D, Date),
  nth0(0, SL, S),
  nth0(0, EL, E),

  datetime_date_time(SDT, Date, S),
  datetime_date_time(EDT, Date, E),

  rfc339Timestamp(SDT, Start),
  rfc339Timestamp(EDT, End),

  listEventsCall(Start, End, _),
  switchState(respond).

performState(listEvents) :-
  currentState(listEvents),

  not(missingEntity(calendarAsk, ora_inceput_relativ)),
  finalEntity(calendarAsk, data, D, M2),
  finalEntity(calendarAsk, ora_inceput_relativ, OIR, M3),

  format(string(Message), 'Listare evenimente ~s ~s.', [M2, M3]),
  assertz(message(Message)),

  log('Insert calendar API call'),

  nth0(0, D, Date),
  nth0(0, OIR, S),
  nth0(1, OIR, E),

  datetime_date_time(SDT, Date, S),
  datetime_date_time(EDT, Date, E),

  rfc339Timestamp(SDT, Start),
  rfc339Timestamp(EDT, End),

  listEventsCall(Start, End, _),
  switchState(respond).

% Store data for event to be created during this state. (mockup event for now)
performState(calendarAdd) :- 
  currentState(calendarAdd),
  % call intent setup function to get final and missing entities
  Entities = [event, data, ora_inceput, ora_final],
  getEntitiesValues(calendarAdd, Entities, false),
  switchState(sendBack).

performState(nil).

% Will reach goal if at least one entity is missing
performState(sendBack) :- 
  currentState(sendBack),
  retractall(message(_)),
  retractall(toBeModified(_, _)),
  hasMissingEntity(),
  switchState(askForUpdate).

% Will reach goal if start and end time are not passed in an ascending order
performState(sendBack) :- 
  currentState(sendBack),
  retractall(message(_)),
  startEndTimeForAddAreCorrect(),
  switchState(askForUpdate).

% Event is fully specified and correct
performState(sendBack) :- 
  currentState(sendBack),

  finalEntity(calendarAdd, event, _, M1),
  finalEntity(calendarAdd, data, _, M2),
  finalEntity(calendarAdd, ora_inceput, _, M3),
  finalEntity(calendarAdd, ora_final, _, M4),

  format(string(Message), 'Evenimentul se nume\u0219te ~s. Data evenimentului este ~s. Ora de \u00eenceput ~s, iar ora de final ~s. E\u0219ti de acord cu ad\u0103ugarea acestuia?', [M1, M2, M3, M4]),
  Final =..[message, Message],
  
  retractall(message(_)),
  assertz(Final),

  switchState(askForApproval).

performState(askForApproval) :-
  currentState(askForApproval),
  retract(message(Message)),
  talkBack(Message),
  switchState(waitForAnswer).

performState(waitForAnswer) :- 
  currentState(waitForAnswer),
  intent(answer),
  switchState(calendarEventDecider).

performState(calendarEventDecider) :- 
  currentState(calendarEventDecider),
  positiveAnswer(),
  Message =..[message, 'Evenimentul va fi ad\u0103ugat.'],
  assertz(Message),
  switchState(calendarEventInsert).

performState(calendarEventInsert) :-
  currentState(calendarEventInsert),
  log('Insert calendar API call'),

  finalEntity(calendarAdd, event, Event, _),
  finalEntity(calendarAdd, data, D, _),
  finalEntity(calendarAdd, ora_inceput, SL, _),
  finalEntity(calendarAdd, ora_final, EL, _),

  nth0(0, D, Date),
  nth0(0, SL, S),
  nth0(0, EL, E),

  datetime_date_time(SDT, Date, S),
  datetime_date_time(EDT, Date, E),

  rfc339Timestamp(SDT, Start),
  rfc339Timestamp(EDT, End),

  insertEventCall(Event, 'Cluj-Napoca', Start, End, _),
  % log(Res),

  switchState(respond).

performState(calendarEventDecider) :- 
  currentState(calendarEventDecider),
  negativeAnswer(),
  Message =..[message, 'Evenimentul nu va fi ad\u0103ugat.'],
  assertz(Message),
  switchState(respond).

performState(respond) :-
  currentState(respond),
  retract(message(Message)),
  
  % cleanup
  retractall(intent(_)),
  retractall(entity(_, _, _)),
  retractall(finalEntity(_, _, _, _)),
  retractall(toBeModified(_, _)),
  retractall(missingEntity(_, _)),

  talkBack(Message),
  switchState(idle).

performState(askForUpdate) :-
  currentState(askForUpdate),
  retract(message(Message)),
  talkBack(Message),
  switchState(waitForUpdate).

performState(waitForUpdate) :-
  currentState(waitForUpdate),
  intent(calendarUpdate),
  switchState(calendarUpdate).

performState(calendarUpdate) :-
  % retract(toBeModified(calendarAdd, Entity)),
  % retract(finalEntity(calendarAdd, Entity, _, Message)),

  % newFinal =..[finalEntity(finalEntity, calendarAdd, Entity, _, Message)],
  % assertz(newFinal),
  currentState(calendarUpdate),

  Entities = [event, data, ora],
  getEntitiesValues(calendarUpdate, Entities, false),

  update(),
  log('Modified event'),
  retractall(intent(_)),
  switchState(sendBack).

update() :- 
  toBeModified(calendarAdd, ora_inceput), 
  entity(calendarUpdate, ora, _),

  retract(missingEntity(calendarAdd, ora_inceput)),
  retract(toBeModified(calendarAdd, ora_inceput)),
  retract(entity(calendarUpdate, ora, _)),
  retract(finalEntity(calendarUpdate, ora, R, M)),

  assertz(finalEntity(calendarAdd, ora_inceput, R, M)),
  log('Update ora_start'), !.

update() :- 
  toBeModified(calendarAdd, ora_final), 
  entity(calendarUpdate, ora, _),

  retract(missingEntity(calendarAdd, ora_final)),
  retract(toBeModified(calendarAdd, ora_final)),
  retract(entity(calendarUpdate, ora, _)),
  retract(finalEntity(calendarUpdate, ora, R, M)),

  assertz(finalEntity(calendarAdd, ora_final, R, M)),
  log('Update ora_final'), !.

update() :- 
  toBeModified(calendarAdd, data), 
  entity(calendarUpdate, data, _),

  retract(missingEntity(calendarAdd, data)),
  retract(toBeModified(calendarAdd, data)),
  retract(finalEntity(calendarUpdate, data, R, M)),
  retract(entity(calendarUpdate, data, _)),

  assertz(finalEntity(calendarAdd, data, R, M)),
  log('Update data'), !.

update() :- 
  toBeModified(calendarAdd, event), 
  entity(calendarUpdate, event, _),

  retract(missingEntity(calendarAdd, event)),
  retract(toBeModified(calendarAdd, event)),
  retract(finalEntity(calendarUpdate, event, R, M)),
  retract(entity(calendarUpdate, event, _)),

  assertz(finalEntity(calendarAdd, event, R, M)),
  log('Update event'), !.

update() :-
  log('Could not update anything').

% Will fail if all entities are filled.
hasMissingEntity() :- 
  missingEntity(calendarAdd, event),       
  Message =..[message, 'Cum vrei s\u0103 se numeasc\u0103 evenimentul?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, event], assertz(Modified).
hasMissingEntity() :- 
  missingEntity(calendarAdd, data),        
  Message =..[message, '\u00een ce dat\u0103 dore\u0219ti s\u0103 fie evenimentul?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, data], assertz(Modified).
hasMissingEntity() :- 
  missingEntity(calendarAdd, ora_inceput), 
  Message =..[message, 'Care dore\u0219ti s\u0103 fie ora de \u00eenceput a evenimentului?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, ora_inceput],  assertz(Modified).
hasMissingEntity() :- 
  missingEntity(calendarAdd, ora_final),   
  Message =..[message, 'Care dore\u0219ti s\u0103 fie ora de final a evenimentului?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, ora_final], assertz(Modified).

startEndTimeForAddAreCorrect() :-
  finalEntity(calendarAdd, ora_inceput, StartL, _),
  finalEntity(calendarAdd, ora_final, EndL, _),

  nth0(0, StartL, Start),
  nth0(0, EndL, End),

  not(time_compare(Start, <, End)),
  % Set message
  Message =..[message, 'Orele de \u00eenceput \u0219i de final nu sunt date corect. Care dore\u0219ti s\u0103 fie ora de \u00eenceput a evenimentului?'],                
  assertz(Message),

  M1 =..[missingEntity, calendarAdd, ora_inceput], assertz(M1),
  M2 =..[missingEntity, calendarAdd, ora_final], assertz(M2),

  Modified =..[toBeModified, calendarAdd, ora_inceput],  
  assertz(Modified),

  % Remove start/end hour so that the state will be reached once again
  retractall(finalEntity(calendarAdd, ora_inceput, _, _)),
  retractall(finalEntity(calendarAdd, ora_final, _, _)).

% We don't want to fail.
persistEntities([]) :- !.
persistEntities([Entity | T]) :-
  atom_string(Name, Entity.get('entity')),
  atom_string(Value, Entity.get('value')),
  intent(Intent),
  Actual =..[entity, Intent, Name, Value],
  assertz(Actual),
  % term_string(Actual, Str),
  % log(Str),
  persistEntities(T).

% For manual testing only.
doAdd() :-
  assertz(intent(calendarAdd)),
  assertz(entity(calendarAdd, event, 'pr\u00e2nz cu Maria')), 
  assertz(entity(calendarAdd, data, joi)), 
  assertz(entity(calendarAdd, ora_inceput, '12')),
  assertz(entity(calendarAdd, ora_final, '13')),
  intentReceived().
  % entity(calendarAdd, ora_final, unu)

% For manual testing only.
doUpdate() :-
  assertz(intent(calendarUpdate)),
  assertz(entity(calendarUpdate, ora, '12')),
  intentReceived().

% For manual testing only.
doAnswer() :-
  assertz(intent(answer)),
  assertz(entity(answer, positive, 'Da')),
  intentReceived().

doAsk() :-
  assertz(intent(calendarAsk)),
  assertz(entity(calendarAsk, data, 'joi')),
  assertz(entity(calendarAsk, ora_inceput_relativ, 'dupa amiaza')),
  intentReceived().

doWeather() :-
  assertz(intent(queryWeather)),
  assertz(entity(queryWeather, timp, 'peste doua ore')),
  assertz(entity(queryWeather, loc, 'in Floresti')),
  intentReceived().

intentEndpointHandler(Request) :-
  http_read_json(Request, Dict, [json_object(dict)]),

  atom_string(Intent, Dict.get('intent')),
  Entities = Dict.get('entities'),

  % Remove old intent
  retractall(intent(_)),
  retractall(entity(answer, _, _)),
  retractall(entity(calendarUpdate, _, _)),

  New =..[intent, Intent],
  assertz(New),
  persistEntities(Entities),

  reply_json(json([message = 'Received intent'])), nl,
  % portray_clause(Request),
  intentReceived().


% Perform states requiring intent as input.
intentReceived() :- intent(X), not(validIntent(X)), log('Invalid intent'), switchState(idle).
intentReceived() :- performState(idle), !.
intentReceived() :- performState(waitForAnswer), !.
intentReceived() :- performState(waitForUpdate), !.
% If no state can be performed, switch to idle.
intentReceived() :- switchState(idle), !.

log(Message) :- format(string(Str), '~s~n', Message), write(Str).

setState(NewState) :-
  currentState(S),
  format(string(Message), 'State switch: ~s -> ~s', [S, NewState]),
  log(Message),
  retractall(currentState(_)),
  New =..[currentState, NewState],
  assertz(New).

switchState(NewState) :- setState(NewState), performState(NewState), !.
switchState(waitForAnswer) :- setState(waitForAnswer), !.
switchState(waitForUpdate) :- setState(waitForUpdate).

% Call to text to speech api
talkBack(Message) :- log(Message).

positiveAnswer() :- entity(answer, positive, _).

negativeAnswer() :- not(entity(answer, _, _)), !.
negativeAnswer() :- not(positiveAnswer()), !.
negativeAnswer() :- entity(answer, negative, _).

% Purely for testing purposes
yes() :-
  retractall(entity(answer, _, _)),
  Answer =..[entity, answer, positive, 'Message'],
  assertz(Answer).

no() :-
  retractall(entity(answer, _, _)),
  Answer =..[entity, answer, positive, 'Message'],
  assertz(Answer).