square_rec(N, S):- square_rec(N, S, N).
square_rec(_, 0, 0).
square_rec(N, S, Count):-
    Count>0,
    Count1 is Count-1,
    square_rec(N, S1, Count1),
    S is S1+N.