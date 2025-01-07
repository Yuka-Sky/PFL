list_sum([], 0).
list_sum([H|T], Sum):-
    list_sum(T, Sum1),
    Sum is Sum1 + H.