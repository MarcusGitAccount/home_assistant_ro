
:- dynamic currentState/1.
:- dynamic entity/3.
:- dynamic intent/1.
:- dynamic event/4.

% Possible intents:
% :- intent(answer) %  answer received from the user of type yes/no
% :- intent(queryWeather)
% :- intent(calendarAdd)
% :- intent(calendarAsk)

currentState(idle).

performState(idle) :- currentState(idle), not(intent(waitForIntent)), intent(I), switchState(I).

% Store data for event to be created during this state. (mockup event for now)
performState(calendarAdd) :- 
  currentState(calendarAdd),
  retractall(event(_, _, _, _)),
  Event =..[event, 'Mockup event', 'Cluj-Napoca', '2020-06-15T13:45:00+03:00', '2020-06-15T15:15:00+03:00'],
  assertz(Event),
  switchState(sendBack).

% Ask the user if the event correponsds to the given request.
performState(sendBack) :- 
  currentState(sendBack),
  % TODO: Time message for timestamps, similarly to weather time.
  event(Title, Location, _, _),
  format(string(Message), 'Will add event ~s at ~s. Is that ok to you?', [Title, Location]),
  talkBack(Message),
  switchState(waitForIntent).

performState(waitForIntent) :- 
  currentState(waitForIntent),
  intent(answer),
  switchState(calendarEventDecider).

performState(calendarEventDecider) :- currentState(calendarEventDecider), positiveAnswer(), talkBack('Event added to calendar.').
performState(calendarEventDecider) :- currentState(calendarEventDecider), negativeAnswer(), talkBack('Will not add event to calendar.').

% Perform states requiring intent as input.
intentReceived() :- 
  performState(idle), !; 
  performState(waitForIntent), !.

% Remove the old intent and its entities from the knowledge base.
intentSetup(Intent) :-
  retractall(intent(_)),
  retractall(entity(_, _, _)),
  % TODO: setup entities too
  % Add new intent to kb
  New =..[intent, Intent],
  assertz(New),
  % Callback for states that require a new intent as input
  intentReceived().

setState(NewState) :-   
  retractall(currentState(_)),
  New =..[currentState, NewState],
  assertz(New).

switchState(NewState) :- setState(NewState), performState(NewState), !.

switchState(waitForIntent) :- setState(waitForIntent).

talkBack(Message) :- write(Message), nl.

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