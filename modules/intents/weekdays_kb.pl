:- dynamic weekday_date/2.

update_weekday_dates(_) :-
    % Remove previously stored values
    retractall(weekday_date(_, _)),

    % Get the date for today
    date_get(today, Today),
    % Store the day of the week corresponding to today
    week_day(Today, WD),
    assertz(weekday_date(WD, Today)),

    % Get the date for following days
    % and store their corresponding days of the week
    date_add(Today, [1 days], NewDate_1),
    week_day(NewDate_1, WD_1),
    assertz(weekday_date(WD_1, NewDate_1)),

    date_add(Today, [2 days], NewDate_2),
    week_day(NewDate_2, WD_2),
    assertz(weekday_date(WD_2, NewDate_2)),

    date_add(Today, [3 days], NewDate_3),
    week_day(NewDate_3, WD_3),
    assertz(weekday_date(WD_3, NewDate_3)),

    date_add(Today, [4 days], NewDate_4),
    week_day(NewDate_4, WD_4),
    assertz(weekday_date(WD_4, NewDate_4)),

    date_add(Today, [5 days], NewDate_5),
    week_day(NewDate_5, WD_5),
    assertz(weekday_date(WD_5, NewDate_5)),

    date_add(Today, [6 days], NewDate_6),
    week_day(NewDate_6, WD_6),
    assertz(weekday_date(WD_6, NewDate_6)).
