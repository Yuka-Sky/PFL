list_perm([], []).
list_perm([X|Xs], [Y|Ys]):-
    find(X, [Y|Ys]),
    list_perm(Xs, Ys).

find(_,[], []).
find(Elm, [Elm|T], Rest):-
    find(Elm, T, Rest).
find(Elm, [H|T1], [H|T2]):-
    Elm \== H,
    find(Elm, T1, T2).