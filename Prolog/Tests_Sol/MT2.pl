:- dynamic dish/3.

% dish(Name, Price, IngredientGrams).
dish(pizza,         2200, [cheese-300, tomato-350]).
dish(ratatouille,   2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread,  1600, [cheese-50, garlic-200]).

:- dynamic ingredient/2.

% ingredient(Name, CostPerGram).
ingredient(cheese,   4).
ingredient(tomato,   2).
ingredient(eggplant, 7).
ingredient(garlic,   6).

%WITHOUT using multiple solution predicates (findall, setof, and bagof), and WITHOUT using any  SICStus library.
count_ingredients(dish(_, _, []), 0).
count_ingredients(Dish, NumIngredients):-
    dish(Dish, _, [_-_|T]),
    count_ingredients(dish(Dish, _, T), N),
    NumIngredients is N+1.

ingredient_amount_cost(Ingredient, Grams, TotalCost):-
    ingredient(Ingredient, Cost),
    TotalCost is Cost * Grams.


dish_profit(Dish, Profit):- dish_profit(Dish, Profit, 0).
dish_profit(dish(_, Price, []), Profit, Acc):-
    Profit is Price-Acc.
dish_profit(Dish, Profit, Acc):-
    dish(Dish, Price, [Ingredient|T]),
    Ingredient = IngName-Quant,
    ingredient(IngName, Cost),
    ingredient_amount_cost(ingredient(IngName, Cost), Quant, TotalCost),
    Acc1 is Acc+TotalCost,
    dish_profit(dish(Name, Price, T), Profit, Acc1).

update_unit_cost(Ingredient, NewUnitCost) :-
    (   ingredient(Ingredient, _) ->
        retract(ingredient(Ingredient, _))  % Remove
    ;   true 
    ),
    assert(ingredient(Ingredient, NewUnitCost)). % Add a new rule

most_expensive_dish(Dish, Price) :-
    dish(Dish, Price, _),
    \+ (dish(_, OtherPrice, _), OtherPrice > Price).
    %This part ensures that no other dish has a higher price than the one we're
    %currently considering. The \+ operator is used to negate the goal,
    %ensuring that the query fails if there is a dish with a higher price.