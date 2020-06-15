
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(date_time)).

:- consult('../calendar/calendar.pl').

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
  currentState(idle),
  switchState(idle).

% Store data for event to be created during this state. (mockup event for now)
performState(calendarAdd) :- 
  currentState(calendarAdd),
  % call intent setup function to get final and missing entities
  switchState(sendBack).

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
  finalEntity(calendarAdd, event, _, M2),
  finalEntity(calendarAdd, event, _, M3),
  finalEntity(calendarAdd, event, _, M4),

  format(string(Message), 'Evenimentul se numește ~s. Data evenimentului este ~s. Ora de început ~s, iar ora de final ~s. Ești de acord cu adăugarea acestuia?', [M1, M2, M3, M4]),
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
  Message =..[message, 'Evenimentul va fi adăugat.'],
  assertz(Message),
  switchState(calendarEventInsert).

performState(calendarEventInsert) :-
  currentState(calendarEventInsert),
  log('Insert calendar API call'),
  switchState(respond).

performState(calendarEventDecider) :- 
  currentState(calendarEventDecider),
  negativeAnswer(),
  Message =..[message, 'Evenimentul nu va fi adăugat.'],
  assertz(Message),
  switchState(respond).

performState(respond) :-
  currentState(respond),
  retract(message(Message)),
  talkBack(Message),
  switchState(idle).

performState(askForUpdate) :-
  currentState(askForApproval),
  retract(message(Message)),
  talkBack(Message),
  switchState(waitForUpdate).

performState(waitForUpdate) :-
  currentState(waitForUpdate),
  intent(calendarUpdate),
  switchState(calendarUpdate).

performState(calendarUpdate) :-
  retract(toBeModified(calendarAdd, Entity)),
  retract(finalEntity(calendarAdd, Entity, _, Message)),

  newFinal =..[finalEntity(finalEntity, calendarAdd, Entity, _, Message)],
  assertz(newFinal),

  currentState(calendarEventUpdate),
  log('Modified event'),
  switchState(sendBack).

% Will fail if all entities are filled.
hasMissingEntity() :- 
  missingEntity(calendarAdd, event),       
  Message =..[message, 'Cum vrei să se numească evenimentul?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, event], assertz(Modified).
hasMissingEntity() :- 
  missingEntity(calendarAdd, data),        
  Message =..[message, 'În ce dată dorești să fie evenimentul?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, data], assertz(Modified).
hasMissingEntity() :- 
  missingEntity(calendarAdd, ora_inceput), 
  Message =..[message, 'Care dorești să fie ora de început a evenimentului?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, ora_inceput],  assertz(Modified).
hasMissingEntity() :- 
  missingEntity(calendarAdd, ora_final),   
  Message =..[message, 'Care dorești să fie ora de final a evenimentului?'], assertz(Message), 
  Modified =..[toBeModified, calendarAdd, ora_final], assertz(Modified).

startEndTimeForAddAreCorrect() :-
  finalEntity(calendarAdd, ora_inceput, Start, _),
  finalEntity(calendarAdd, ora_final, End, _),
  not(time_compare(Start, <, End)),
  % Set message
  Message =..[message, 'Orele de început și de final nu sunt date corect. Care dorești să fie ora de început a evenimentului?'],                
  assertz(Message),

  % Remove start/end hour so that the state will be reached once again
  retractall(finalEntity(calendarAdd, ora_inceput, _)),
  retractall(finalEntity(calendarAdd, ora_final, _)).

% We don't want to fail.
persistEntities([]) :- !.
persistEntities([Entity | T]) :-
  atom_string(Name, Entity.get('entity')),
  atom_string(Value, Entity.get('value')),
  intent(Intent),
  Actual =..[entity, Intent, Name, Value],
  assertz(Actual),
  persistEntities(T).

intentEndpointHandler(Request) :-
  http_read_json(Request, Dict, [json_object(dict)]),

  atom_string(Intent, Dict.get('intent')),
  Entities = Dict.get('entities'),

  % Remove old intent
  retractall(intent(_)),
  % retractall(entity(_, _, _)),

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