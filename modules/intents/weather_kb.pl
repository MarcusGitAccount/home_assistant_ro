relative(queryWeather, timp, 'dimineata', R, Mesaj) :-
    % Get hour component from current time
    time_get(now, time(H, _, _)),  
    (
        % If it's between 8 AM and 12 PM, just get current weather
        between(8, 12, H)
        -> 
        R = ['current'], % 0 hour offset
        Mesaj = 'azi diminea\u021ba'
        ;
        % else, compute how many hours until 10 AM
        time_interval(time(10, 0, _), time(H, 0, _), hours(Diff)),
        (
            Diff < 0 
            ->
            % A negative number means current time is between 12 and 24, so we need tomorrow's morning weather
            NewDiff is 24 + Diff,
            R = ['hourly', NewDiff],
            Mesaj = 'm\u00e2ine diminea\u021ba'
            ;
            % A positive number means it's night time, so we need today's morning weather
            R = ['hourly', Diff],
            Mesaj = 'azi diminea\u021ba'
        )    
    ).


relative(queryWeather, timp, 'dupa amiaza', R, Mesaj) :-
    % Get hour component from current time
    time_get(now, time(H, _, _)),  
    (
        % If it's between 12 PM and 5 PM, just get current weather
        between(12, 17, H)
        -> 
        R = ['current'], % 0 hour offset
        Mesaj = 'azi dup\u0103-amiaz\u0103'
        ;
        % else, compute how many hours until 2 PM
        time_interval(time(14, 0, _), time(H, 0, _), hours(Diff)),
        (
            Diff < 0 
            ->
            % A negative number means current time is between 18 and 24, so we need tomorrow's afternoon weather
            NewDiff is 24 + Diff,
            R = ['hourly', NewDiff],
            Mesaj = 'm\u00e2ine dup\u0103-amiaz\u0103'
            ;
            % A positive number means it's nighttime or morning, so we need today's afternoon weather
            R = ['hourly', Diff],
            Mesaj = 'azi dup\u0103-amiaz\u0103'
        )    
    ).

relative(queryWeather, timp, 'seara', R, Mesaj) :-
    % Get hour component from current time
    time_get(now, time(H, _, _)),  
    (
        % If it's between 6 PM and 10 PM, just get current weather
        between(18, 22, H)
        -> 
        R = ['current'], % 0 hour offset
        Mesaj = 'azi seara'
        ;
        % else, compute how many hours until 8 PM
        time_interval(time(14, 0, _), time(H, 0, _), hours(Diff)),
        (
            Diff < 0 
            ->
            % A negative number means current time is between 22 and 24, so we need tomorrow's evening weather
            NewDiff is 24 + Diff,
            R = ['hourly', NewDiff],
            Mesaj = 'm\u00e2ine seara'
            ;
            % A positive number means it's nighttime, morning or daytime, so we need today's evening weather
            R = ['hourly', Diff],
            Mesaj = 'azi seara'
        )    
    ).

% Alternative turns of phrase
relative(queryWeather, timp, 'dupa-masa', R, Mesaj) :- relative(queryWeather, timp, 'dupa amiaza', R, Mesaj).
relative(queryWeather, timp, 'dupa masa', R, Mesaj) :- relative(queryWeather, timp, 'dupa amiaza', R, Mesaj).
relative(queryWeather, timp, 'dupa-amiaza', R, Mesaj) :- relative(queryWeather, timp, 'dupa amiaza', R, Mesaj).


relative(queryWeather, timp, 'peste o ora', R, Mesaj) :- R = ['hourly', 1], Mesaj = 'peste o or\u0103'.
relative(queryWeather, timp, 'peste doua ore', R, Mesaj) :- R = ['hourly', 2], Mesaj = 'peste dou\u0103 ore'.
relative(queryWeather, timp, 'azi', R, Mesaj) :- R = ['current'], Mesaj = 'azi'.
relative(queryWeather, timp, 'maine', R, Mesaj) :- R = ['daily', 1 , 'day'], Mesaj = 'm\u00e2ine'.
relative(queryWeather, timp, 'maine dimineata', R, Mesaj) :- R = ['daily', 1, 'morn'], Mesaj = 'm\u00e2ine diminea\u021ba'.
relative(queryWeather, timp, 'maine dupa masa', R, Mesaj) :- R = ['daily', 1, 'day'], Mesaj = 'm\u00e2ine dup\u0103-amiaz\u0103'.
relative(queryWeather, timp, 'saptamana viitoare', R, Mesaj) :- R = ['daily', 7, 'day'], Mesaj = 's\u0103pt\u0103m\u00e2na viitoare'.


relative(queryWeather, timp, 'luni', R, Mesaj) :-
    % Compute difference between next monday's date and today's date
    weekday_date('Monday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 'luni'.

relative(queryWeather, timp, 'marti', R, Mesaj) :-
    weekday_date('Tuesday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 'mar\u021bi'.

relative(queryWeather, timp, 'miercuri', R, Mesaj) :-
    weekday_date('Wednesday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 'miercuri'.

relative(queryWeather, timp, 'joi', R, Mesaj) :-
    weekday_date('Thursday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 'joi'.

relative(queryWeather, timp, 'vineri', R, Mesaj) :-
    weekday_date('Friday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 'vineri'.

relative(queryWeather, timp, 'sambata', R, Mesaj) :-
    weekday_date('Saturday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 's\u00e2mb\u0103t\u0103'.

relative(queryWeather, timp, 'duminica', R, Mesaj) :-
    weekday_date('Sunday', D2),
    date_get(today, D1),
    date_interval(D2, D1, D days),
    R = ['daily', D, 'day'],
    Mesaj = 'duminic\u0103'.



relative(queryWeather, loc, 'in Cluj-Napoca', R, Mesaj) :-
    R = ['46.770439', '23.591423'],
    Mesaj = '\u00een Cluj-Napoca'.

relative(queryWeather, loc, 'in Floresti', R, Mesaj) :-
    R = ['46.744022', '23.482088'],
    Mesaj = '\u00een Flore\u0218ti'.
relative(queryWeather, loc, 'in Bucuresti', R, Mesaj) :-
    R = ['44.439663', '26.096306'],
    Mesaj = '\u00een Bucure\u0218ti'.

relative(queryWeather, loc, 'afara', R, Mesaj) :- relative(queryWeather, loc, 'in Cluj-Napoca', R, Mesaj).
relative(queryWeather, loc, 'afara', R, Mesaj) :- relative(queryWeather, loc, 'in Cluj-Napoca', R, Mesaj).
