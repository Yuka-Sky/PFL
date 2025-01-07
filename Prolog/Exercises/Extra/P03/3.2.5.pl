del_dups([], []).
del_dups([X|Xs], [X|Ys]):-
    del_all(X, Xs, List),
    del_dups(List, Ys).

del_all(_,[], []).
del_all(Elm, [Elm|T], Rest):-
    del_all(Elm, T, Rest).
del_all(Elm, [H|T1], [H|T2]):-
    Elm \== H,
    del_all(Elm, T1, T2).




/*del_dups([], []).
del_dups([X|Xs], [X|Ys]) :-
    exclude(=(X), Xs, Rest),
    del_dups(Rest, Ys).

The exclude/3 predicate is a built-in predicate in Prolog
that filters a list based on a certain condition. In this case,
exclude/3 is used to filter out all occurrences of X from the list Xs.

It creates an anonymous predicate that checks whether its
argument is equal to X. So, =(X) is a predicate that succeeds
when its argument is equal to X and fails otherwise.*/