pow_rec(X, Y, P):- pow_rec(X, Y, P, 1).
pow_rec(_, 0, Acc, Acc). %X^0 is 1
pow_rec(X, Y, P, Acc):-
    Y>0,
    Y1 is Y-1,
    Acc1 is Acc*X,
    pow_rec(X, Y1, P, Acc1).