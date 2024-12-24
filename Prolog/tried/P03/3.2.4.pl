dell_all(_, [], []).
dell_all(Elm, [Elm|T1], T2):-
    dell_all(Elm, T1, T2).
dell_all(Elm, [X|Xs], [X|Ys]):-
    Elm \== X,
    dell_all(Elm, Xs, Ys).

del_all_list([], List, List).
del_all_list([Elem|RestElems], List1, List2) :-
    dell_all(Elem, List1, TempList),
    del_all_list(RestElems, TempList, List2).