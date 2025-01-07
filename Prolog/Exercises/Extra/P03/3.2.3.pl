dell_all(_, [], []).
dell_all(Elm, [Elm|T1], T2):-
    dell_all(Elm, T1, T2).
dell_all(Elm, [X|Xs], [X|Ys]):-
    Elm \== X,
    dell_all(Elm, Xs, Ys).