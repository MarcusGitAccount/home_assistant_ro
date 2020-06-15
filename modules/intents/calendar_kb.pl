% Same results should be valid whether we want to query the calendar
% or add a new event
relative(calendarAdd, Y, Val, R, Mesaj) :- relative(calendarAsk, Y, Val, R, Mesaj), !.

relative(calendarAsk, event, V, V, V).

relative(calendarAsk, data, 'azi', R, Mesaj) :-
    date_get(today, Today),
    R = [Today],
    Mesaj = 'azi'.

relative(calendarAsk, data, 'maine', R, Mesaj) :-
    date_get(tomorrow, Tomorrow),
    R = [Tomorrow],
    Mesaj = 'm\u00e2ine'.

relative(calendarAsk, data, 'poimaine', R, Mesaj) :-
    date_get(today, Today),
    % Add 2 days to today's date
    date_add(Today, days(2), DayAfterTomorrow),
    R = [DayAfterTomorrow],
    Mesaj = 'poim\u00e2ine'.

relative(calendarAsk, data, 'peste doua zile', R, Mesaj) :- relative(calendarAsk, data, 'poimaine', R, Mesaj).

relative(calendarAsk, data, 'peste trei zile', R, Mesaj) :-
    date_get(today, Today),
    % Add 3 days to today's date
    date_add(Today, days(3), ThreeDaysAfter),
    R = [ThreeDaysAfter],
    Mesaj = 'peste trei zile'.

relative(calendarAsk, data, 'peste o saptamana', R, Mesaj) :-
    date_get(next_week, OneWeekLater),
    R = [OneWeekLater],
    Mesaj = 'peste o s\u0103pt\u0103m\u00e2n\u0103'.

relative(calendarAsk, data, 'saptamana viitoare', R, Mesaj) :- relative(calendarAsk, data, 'peste o saptamana', R, Mesaj).



relative(calendarAsk, data, 'in weekend', R, Mesaj) :-
    weekday_date('Friday', Friday),
    % Add 3 days to next Friday's date
    date_add(Friday, days(3), Sunday),
    R = [Friday, Sunday],
    Mesaj = '\u00een weekend'.



relative(calendarAsk, data, 'luni', R, Mesaj) :-
    weekday_date('Monday', Monday),
    R = [Monday],
    Mesaj = 'luni'.

relative(calendarAsk, data, 'marti', R, Mesaj) :-
    weekday_date('Tuesday', Tuesday),
    R = [Tuesday],
    Mesaj = 'mar\u021bi'.

relative(calendarAsk, data, 'miercuri', R, Mesaj) :-
    weekday_date('Wednesday', Wednesday),
    R = [Wednesday],
    Mesaj = 'miercuri'.

relative(calendarAsk, data, 'joi', R, Mesaj) :-
    weekday_date('Thursday', Thursday),
    R = [Thursday],
    Mesaj = 'joi'.

relative(calendarAsk, data, 'vineri', R, Mesaj) :-
    weekday_date('Friday', Friday),
    R = [Friday],
    Mesaj = 'vineri'.

relative(calendarAsk, data, 'sambata', R, Mesaj) :-
    weekday_date('Saturday', Saturday),
    R = [Saturday],
    Mesaj = 's\u00e2mb\u0103t\u0103'.

relative(calendarAsk, data, 'duminica', R, Mesaj) :-
    weekday_date('Sunday', Sunday),
    R = [Sunday],
    Mesaj = 'duminic\u0103'.





relative(calendarAsk, ora_inceput_relativ, 'dupa-masa', R, Mesaj) :-
    R = [time(12, 0, 0), time(18, 0, 0)],
    Mesaj = ['dup\u0103-mas\u0103'].

relative(calendarAsk, ora_inceput_relativ, 'dupa pranz', R, Mesaj) :-
    R = [time(12, 0, 0), time(14, 0, 0)],
    Mesaj = ['dup\u0103 pr\u00e2nz'].

relative(calendarAsk, ora_inceput_relativ, 'dimineata', R, Mesaj) :-
    R = [time(7, 0, 0), time(12, 0, 0)],
    Mesaj = ['diminea\u021ba'].

relative(calendarAsk, ora_inceput_relativ, 'deseara', R, Mesaj) :-
    R = [time(18, 0, 0), time(23, 0, 0)],
    Mesaj = ['desear\u0103'].

relative(calendarAsk, ora_inceput, '12', R, Mesaj) :- R = [time(12, 0, 0)], Mesaj = '12'.
relative(calendarAsk, ora_inceput, 'doisprezece', R, Mesaj) :- R = [time(12, 0, 0)], Mesaj = '12'.
relative(calendarAsk, ora_inceput, '13', R, Mesaj) :- R = [time(13, 0, 0)], Mesaj = '13'.
relative(calendarAsk, ora_inceput, 'treisprezece', R, Mesaj) :- R = [time(13, 0, 0)], Mesaj = '13'.
relative(calendarAsk, ora_inceput, 'unu', R, Mesaj) :- R = [time(13, 0, 0)], Mesaj = '13'.
relative(calendarAsk, ora_inceput, '1', R, Mesaj) :- R = [time(13, 0, 0)], Mesaj = '13'.


relative(calendarAsk, ora_final, 'unu', R, Mesaj) :- R = [time(13, 0, 0)], Mesaj = '13'.

relative(calendarAsk, ora_final, Ora, R, Mesaj) :- relative(calendarAsk, ora_inceput, Ora, R, Mesaj).