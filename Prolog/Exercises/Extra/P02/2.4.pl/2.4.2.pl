sum_rec(0, 0).
sum_rec(N, Sum):-
	N>0,
	N1 is N-1,
	sum_rec(N1, Sum1),
	Sum is Sum1 + N.