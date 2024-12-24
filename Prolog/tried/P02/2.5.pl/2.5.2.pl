sum_rec(N, Sum):- sum_rec(N, Sum, 0).
sum_rec(0, Acc, Acc).
sum_rec(N, Sum, Acc):-
	N>0,
	N1 is N-1,
	Acc1 is Acc + N,
	sum_rec(N1, Sum, Acc1).