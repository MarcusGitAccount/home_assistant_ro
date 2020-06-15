:- use_module(library(date_time)).

:- dynamic intent/1.
:- multifile relative/5.
:- dynamic entity/3.
:- dynamic finalEntity/4.
:- dynamic missingEntity/2.

% intent(queryWeather).

:- consult('weekdays_kb.pl').
:- consult('calendar_kb.pl').
:- consult('weather_kb.pl').

% Get dates and weekday names for today and the next 6 days
:- update_weekday_dates(_).

% Specify default values for entities
default(queryWeather, loc, ['46.770439', '23.591423'], '\u00een Cluj-Napoca'). 
default(queryWeather, timp, ['current'], 'acum').
default(calendarAsk, ora_inceput, [time(0, 0, 0)], 'miezul nop\u021bii').
default(calendarAsk, ora_final, [time(23, 59, 59)], 'sfarsitul zilei').
default(calendarAsk, data, R, M) :- relative(calendarAsk, data, 'azi', R, M).
default(calendarAdd, Y, T, M) :- default(calendarAsk, Y, T, M).

% entity(calendarAdd, event, 'Nothing').
% entity(calendarAdd, ora_final, '12').

% to change parameter names
relative(I, E, V) :-
  relative(I, E, V, R, M),
  assertz(finalEntity(I, E, R, M)).

% relative(queryWeather, timp, "dupa amiaza", R). % ora 14 default 

%! getEntitiesValues(-Intent, -Entities, -ReplaceMissing) is det.
%
%  fill in values of current intent's entities
%  and store them as dynamic finalEntity predicates.
%
%  Entities is a list of entities associated with Intent.
%  
%  if ReplaceMissing is true, replace missing entities with default values
%  and store them as dynamic finalEntity predicates.
%  else, store them as dynamic missingEntity predicates.
getEntitiesValues(_, [], _) :- !.
getEntitiesValues(Intent, [Entity|Es], ReplaceMissing) :-
  entity(Intent, Entity, Value),
  !,
  relative(Intent, Entity, Value),
  getEntitiesValues(Intent, Es, ReplaceMissing).
getEntitiesValues(Intent, [Entity|Es], ReplaceMissing) :-
  (
    %if
    ReplaceMissing
    -> %then
    default(Intent, Entity, R, Mesaj),
    assertz(finalEntity(Intent, Entity, R, Mesaj))
    ; %else
    assertz(missingEntity(Intent, Entity))
  ),
  getEntitiesValues(Intent, Es, ReplaceMissing).


%! cleanEntities() is det.
%  removes all existing dynamic finalEntity and missingEntity predicates from knowledge base
cleanEntities() :- 
  retractall(missingEntity(_, _)),
  retractall(finalEntity(_, _, _, _)).


% used for mapping purposes
getEntity(value(E, _), E).

doIntent(Intent, R) :-
  intent(Intent),
  % transform relative entities(weekend date, noon/evening/morning time limits)

  % suisute with default where possible
  findall(value(E, V), entity(Intent, E, V), Existing),
  maplist(getEntity, Existing, Entities),
  findall(value(E, V), (default(Intent, E, V), not(member(E, Entities))), Defaults),
  append(Existing, Defaults, R).
  % check for entities ambiguity(there is not a default for each missing entity) 
  % and state conflicts
  % if so, ask further questions

  % update state if required(not for this intent yet)

relative(calendarUpdate, event, Data, R, Mesaj) :- relative(calendarAsk, event, Data, R, Mesaj).
relative(calendarUpdate, data, Data, R, Mesaj) :- relative(calendarAsk, data, Data, R, Mesaj).
relative(calendarUpdate, ora, Data, R, Mesaj) :- relative(calendarAsk, ora_inceput, Data, R, Mesaj).