
:- dynamic intent/1.
:- dynamic relative/4.
:- dynamic entity/3.
:- dynamic default/3.

intent(queryWeather).

relative(queryWeather, timp, 'dimineata', R, Mesaj).     % ora 10 default 
% relative(queryWeather, timp, "dupa amiaza", R). % ora 14 default 

% entity(queryWeather, timp, 1593006198).
% entity(queryWeather, loc, "Oradea").

default(queryWeather, loc, "Cluj-Napoca").
default(queryWeather, timp, Current) :- get_time(Current). % unix time timestamp as float number [measures seconds]

% transform(queryWeather, timp, azi, Time) :- 

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