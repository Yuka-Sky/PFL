% Q1
same_saga(Movie1, Movie2):-
	movie(Movie1, _Y1, SagaID, _D1, _I1, _Di1, _C1, _Cs1),
	movie(Movie2, _Y2, SagaID, _D2, _I2, _Di2, _C2, _Cs2),
	Movie1 \= Movie2.

% Notes:
% - the verification that the movies are different should only be made after ensuring they are instanciated


% Q2
movie_from_saga(Movie, Saga):-
	saga(SagaID, Saga, _N, _Creator),
	movie(Movie, _Y, SagaID, _Dur, _I, _D, _C, _Cs).


% Q3
saga_best_movie(Saga, Movie):-
	saga(SagaID, Saga, _N, _Creator),
	movie(Movie, _Y1, SagaID, _Dur1, IMDB, _Dir1, _Comp1, _Cast1),
	\+(( movie(_M2, _Y2, SagaID, _Dur2, IM2, _Dir2, _Comp2, _Cast2), IM2 > IMDB )).
% ensures there is no movie with a higher score than the one being considered, helping identify the best movie in the saga.
% Notes:
% - identical to exercise 8.a) from sheet P02 (we just have to deny the existence of a movie from the same saga with a larger classification)


% Q4
add_movie_to_saga(Saga, Movie, Year, Duration, Score, Director, Composer, Cast):-
	saga(SagaID, Saga, _N, _C),
	movie(Movie, Year, SagaID, Duration, Score, Director, Composer, Cast), !, fail.
add_movie_to_saga(Saga, Movie, Year, Duration, Score, Director, Composer, Cast):-
	retract( saga(SagaID, Saga, OldN, Creator) ),
	NewN is OldN + 1,
	assertz( saga(SagaID, Saga, NewN, Creator) ),
	assertz( movie(Movie, Year, SagaID, Duration, Score, Director, Composer, Cast) ).

% Notes:
% - If the movie already exists, no change should be made
% - In case it does not exist (and only if it does not exist), the movie should be inserted and the saga updated


% Q5
movies_from_saga(Saga, Movies):-
	saga(SagaID, Saga, _N, _C),
	movies_from_saga(SagaID, [], Movies).
movies_from_saga(SagaID, Movies, Final):-
	movie(Movie, Year, SagaID, _Dur, _Score, _Dir, _Comp, _Cast),
	\+ member(Year-Movie, Movies), !, %  ensures no duplicate movies are added to the final list by checking if a Year-Movie combination is already in the list.
	movies_from_saga(SagaID, [Year-Movie|Movies], Final).
movies_from_saga(_SagaID, AllMovies, Movies):-
	sort(AllMovies, SortedMovies),
	remove_year(SortedMovies, Movies).

remove_year([], []).
remove_year([_Y-Movie | T], [Movie | T2]):-
	remove_year(T, T2).

% Notes:
% - this is a possible solution to obtain all movies (similar to the one presented in slide 3 from slides PL5)
% - another solution would be to use assert/retract (similar to the one presented in slide 17 from slides PL6)
% - it is necessary to order by year (hence the pairs Year-Movie), since there is no guarantee that movies are ordered in the knowledge base
% - ordering can be achieved using either sort or keysort (built-in predicates presented in slide 54 from slides PL2)
% - note that the cut is necessary to avoid obtaining additional (incomplete) answers via backtracking


% Q6
saga_cast(Saga, Cast):-
	saga(SagaID, Saga, _N, _Cr),
	findall(Actor, ( movie(_T, _Y, SagaID, _Dur, _Sc, _Dir, _Comp, Cast), member(Actor, Cast) ), List),
	sort(List, Cast).

% Notes:
% - the sort/2 predicate orders a list, removing duplicates, thus obtaining the desired result (no duplicates)
% - alternatively, the setof predicate, which includes that functionality, could be used
% - in the case of using setof, it is necessary to use existential quantifyers for all remaining variables (see slides 10 and 11 from slides PL5)


% Q7
sample_cast(Saga, SampleCast):-
	saga_cast(Saga, Cast),
	sample(Cast, SampleCast).

sample([], []).
sample([E], [E]).
sample([A,_B|T], [A|T2]):-
	sample(T, T2).

% Notes:
% - it is necessary to take into account cases when the original list (as returned by saga_cast/2) has an even / odd number of elements
% - a findall could be used to find all members of Cast in odd positions: findall(Actor, (nth1(Idx,Cast,Actor), Idx mod 2 =:= 1), SampleCast)


% Q8
composer_rating(Composer, AvgScore):-
	findall(Score, movie(_M, _Y, _SID, _D, Score, _Dir, Composer, _Cs), Scores),
	length(Scores, Len),
	sumlist(Scores, Sum),
	AvgScore is Sum / Len.


% Q9
% When the third argument is instantiated and is equal to the second argument, the predicate always succeeds, even if that is not the most successful composer.
% Notes: identical problem to the one presented in slide 12 from slides PL3


% Q10
% Red. If removed, Composer2 would always be a possible answer.


% Q11
most_successful_composer(Composer1, Composer2, Composer1):-
	composer_rating(Composer1, S1),
	composer_rating(Composer2, S2),
	S1 >= S2.
most_successful_composer(Composer1, Composer2, Composer2):-
	composer_rating(Composer1, S1),
	composer_rating(Composer2, S2),
	S2 >= S1.

% Notes:
% - when both composers have identical ratings, both are possible results, via backtracking (non-discrimination)


% Q12
% :-op(500, xfx, composed_for).


% Q13
Composer composed_for Movie:- 
	movie(Movie, _Year, _Saga, _Dur, _Score, _Dir, Composer, _Cast).

% Notes:
% - similar to the example presented in slide 20 from slides PL7


% Q14
connected_degree(Person1, Person2, Degree):-
	connected_degree_bfs([ [Person1] ], Person2, Degree).

connected_degree_bfs([ [Person1|R] | _], Person1, Degree):- !,
	length(R, Degree).
connected_degree_bfs([ [Person1|R] | T ], Person2, Degree):-
	setof(Next, ( connected(Person1, Next),
		      \+ (member(Next, [Person1|R])) ), List),
	append_all(List, [Person1|R], ToSee),
	append(T, ToSee, NextList),
	connected_degree_bfs(NextList, Person2, Degree).

append_all([], _List, []).
append_all([H|T], List, [ [H|List] |NT]):-
	append_all(T, List, NT).

% Notes:
% - This is a breadth-first search, saving the path to determine its size
% - Explicitely said during the lecture (slide 18 from slides PL5) that this would be a good exercise for the test
% - A similar implementation is also requested in exercise 4 from sheet P05


% Q15
composed_for_saga(Composer, Saga):-
	nonvar(Composer), !,
	movie(_T, _Y, SagaID, _D, _Sc, _Dir, Composer, _),
	saga(SagaID, Saga, _N, _C).
composed_for_saga(Composer, Saga):-
	saga(SagaID, Saga, _N, _C),
	movie(_T, _Y, SagaID, _D, _Sc, _Dir, Composer, _).

% Notes:
% - For efficiency, search should always start with the instantiated arguments (as seen in slide 5 from slides PL7)
% - Note that the double definition requires a cut to prevent solution duplication
% - Due to a lapse in the code (non_var vs nonvar), no penalties will be applied to this question

% Q16
% The same results would be obtained but in a different order.

% Q17
% maplist(predX, List1, List2).