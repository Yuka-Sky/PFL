% Q1
count_ingredients(Dish, N):-
	dish(Dish, _Cost, L),
	length(L, N).

% Q2
ingredient_amount_cost(Ingredient, Grams, TotalCost):-
    ingredient(Ingredient, Cost),
    TotalCost is Cost * Grams.

% Q3
dish_profit(Dish, Profit):-
	dish(Dish, Price, L),
	dish_cost(L, TotalCost),
	Profit is Price - TotalCost.
dish_cost([], 0).
dish_cost([Ingredient-Grams|Is], TotalCost):-
	ingredient_amount_cost(Ingredient, Grams, Cost),
	dish_cost(Is, TotalCost1),
	TotalCost is TotalCost1 + Cost.

% Q4
update_unit_cost(Ingredient, NewUnitCost):-
	retract( ingredient(Ingredient, _OldCost) ), !,
	assert( ingredient(Ingredient, NewUnitCost) ).
update_unit_cost(Ingredient, NewUnitCost):-
	assert( ingredient(Ingredient, NewUnitCost) ).

% Q5
most_expensive_dish(Dish, Price):-
	dish(Dish, Price, _),
	\+((
	    dish(_, Price1, _),
	    Price1 > Price
	)). % ensure that there is no dish with a higher price than the one found in the first goal.

% Q6
consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks):-
	append(Prefix, [Ingredient-Quant | Suffix], IngredientStocks),
	NewQuant is Quant - Grams,
	NewQuant >= 0,
	append(Prefix, [Ingredient-NewQuant | Suffix], NewIngredientStocks).


% Q7 - Implement count_dishes_with_ingredient(+Ingredient, ?N), which determines how many dishes use the given ingredient.
count_dishes_with_ingredient(Ingredient, N):-
	gather_dishes_with_ingredient(Ingredient, [], Dishes),
	length(Dishes, N).
	
gather_dishes_with_ingredient(Ingredient, Acc, L):-
	dish(Dish, _, Ings), 
	\+ member(Dish, Acc),%checks whether the Dish has already been added to the accumulated list Acc. If it is already in Acc, 
	member(Ingredient-_Amnt, Ings), %the negation fails and that dish will not be considered again. If it is not in Acc, the negation succeeds, and the dish is processed.
	!,
	gather_dishes_with_ingredient(Ingredient, [Dish|Acc], L).
gather_dishes_with_ingredient(_, L, L).


% Q8 - Implement list_dishes(?DishIngredients), which returns a list of pairs Dish-ListOfIngredients.
list_dishes(DishIngredients):-
	findall(D-L, ( dish(D, _, _), list_ingredients(D, L) ), DishIngredients).
list_ingredients(Dish, Ingredients):-
	dish(Dish, _, L),
	findall(Ingredient, member(Ingredient-_Amnt, L), Ingredients).


% Q9 - Implement most_lucrative_dishes(?Dishes), which returns the restaurant’s dishes, sorted by decreasing amount of profit. In case of a tie, any order of the tied dishes will be accepted.
most_lucrative_dishes(Dishes):-
	setof(Profit-Dish, dish_profit(Dish, Profit), Costs),
	reverse(Costs, CostsInv),
	findall(Dish, member(_-Dish, CostsInv), Dishes).


% Q10-11
predX(S, D, NewS):-
	dish(D, _, L),
	predY(L, S, NewS).

predY([], L, L).
predY([I-Gr|Is], S, NewS):-
	!,
	consume_ingredient(S, I, Gr, S1),
	predY(Is, S1, NewS).

% Q11 - The cut present in the recursive clause of predY/3 is green. True or false? - True

% Q12-
predZ:-
	read(X),
	X =.. [_|B],
	length(B, N),
	write(N), nl.

% Q12 - Explain concisely (in one sentence) what predZ/0 does.
% predZ/0 prompts the user the write a term and prints its arity in the console.

% Q13 - By using an extra argument, one can rewrite certain recursive predicates so that they00 are tail-recursive.

% Q14 - Consider the following statements about difference lists.
% A - The [a,b|T]\T difference list is equivalent to [a,b].
% B - Difference lists provide O(1) access to an uninstantiated prefix of a list.
% C - Using difference lists, we can compute the length of a list's prefix in constant time (O(1)).
% Which statements are correct? - Only A



%G1
edge(g1, br, o).
edge(g1, br, ni).
edge(g1, o, ni).
edge(g1, o, c).
edge(g1, o, h).
edge(g1, h, c).
edge(g1, h, n).
edge(g1, n, he).
edge(g1, c, he).

% G2
edge(g2, br, h).
edge(g2, br, ni).
edge(g2, h, ni).
edge(g2, h, o).
edge(g2, h, c).
edge(g2, o, c).
edge(g2, o, n).
edge(g2, n, he).
edge(g2, c, he).
edge(g2, cl, he).

% Q21 - Implement common_edges(+G1, +G2, ?L), which, given the identifiers of two graphs (G1 and G2), computes their list of common edges.]
con(G, X-Y):-
	edge(G, X, Y).
con(G, X-Y):-
	edge(G, Y, X).

common_edges(G1, G2, Edges) :-
    findall(V1-V2, ( edge(G1, V1, V2), con(G2, V1-V2) ), Edges).


% Q22 - Implement common_subgraphs(+G1, +G2, ?Subgraphs), which determines the list of vertices of each common subgraph of both input graphs. Any order of the subgraphs and of the vertices will be accepted.
common_subgraphs(G1, G2, Subgraphs):-
	common_edges(G1, G2, Edges),
	common_subgraphs_aux(Edges, Subgraphs).
	
common_subgraphs_aux([], []).
common_subgraphs_aux([V1-V2|Es], [SGNoDups|SGs]):-
	next_subgraph([V1,V2], Es, NewEs, SG),
	sort(SG, SGNoDups), % remove duplicate nodes
	common_subgraphs_aux(NewEs, SGs).

adjacent(V, V-_).
adjacent(V, _-V).

next_subgraph(Vs, Es, Es, Vs):-
	\+((
	    member(V, Vs),
	    select(E, Es, _),
	    adjacent(V, E)
	)), !.
next_subgraph(Vs, Es, NewEs, SG):-
	member(V, Vs),
	select(V1-V2, Es, Es1),
	adjacent(V, V1-V2),
	!,
	next_subgraph([V1,V2|Vs], Es1, NewEs, SG).
