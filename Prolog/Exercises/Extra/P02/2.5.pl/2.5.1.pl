factorial(N, F):- factorial(N, F, 1).
factorial(0, Acc, Acc).
factorial(N, F, Acc):- N>0,
	N1 is N-1,
	Acc1 is N*Acc,
	factorial(N1, F, Acc1).