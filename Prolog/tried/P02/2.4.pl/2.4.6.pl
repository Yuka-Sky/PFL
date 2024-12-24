is_prime(X) :-
    X > 1,
    is_prime(X, 2).

is_prime(X, Divisor) :-
    Divisor * Divisor > X,
    !.

is_prime(X, Divisor) :-
    X mod Divisor =\= 0,
    NextDivisor is Divisor + 1,
    is_prime(X, NextDivisor).

is_prime(X):-
    X>0,
    X1 is X-1,
    X mod X1 =\= 0,
    