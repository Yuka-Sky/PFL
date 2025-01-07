 pow_rec(_, 0, 1). %X^0 is 1
pow_rec(X, Y, P):-
    Y>0,
    Y1 is Y-1,
    pow_rec(X, Y1, P1),
    P is P1*X.