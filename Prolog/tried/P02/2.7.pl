ancestor_of(X, Y):-
    parent(X, Z),
    ancestor(Z, Y).

descendent_of(X, Y):-
    parent(Z, X),
    descendent_of(Z, Y).

