list_prod([], 1).
list_prod([H|T], Prod):-
    list_prod(T, Prod1),
    Prod is Prod1*H.