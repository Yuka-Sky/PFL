invert([], []).
invert([X|Xs], List):-
    invert(Xs, List1),
     append(List1, [X], List).