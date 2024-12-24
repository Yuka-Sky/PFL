inner_product([], [], 0).
inner_product([H1|T1], [H2,|T2], Result):-
    inner_product(T1, T2, Result1),
    Result is Result1 + (H1*H2).