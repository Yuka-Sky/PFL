gcd(X, 0, X).
gcd(X, Y, G):-
    Y>0,
    X1 is X mod Y,
    gcd(Y, X1, G).

lcm(X, Y, M):-
    gcd(X, Y, G),
    M is (X*Y)//G.