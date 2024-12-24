del_one(_, [], []).
del_one(Elm, [Elm|T], T).
del_one(Elm, [H|T1], [H|T2]):-
    Elm \== H,
    del_one(Elm, T1, T2).