:- use_module(library(lists)).
:- use_module(library(between)).
:- ensure_loaded('score.pl').

% value(+GameState, +Player, -Value)
% Evaluates how favorable the GameState is to the Player.
value(game_state(_, _, Board1, _, _, Board2, _, _), player1, Value) :-
    sum_score(Board1, x, Player11Score),  
    sum_score(Board1, o, Player12Score),
    sum_score(Board2, x, Player21Score),
    sum_score(Board2, o, Player22Score),  
    SafeDenominator is max(Player21Score + Player22Score, 1), 
    Value is (Player11Score + Player12Score) / SafeDenominator.

value(game_state(_, _, Board1, _, _, Board2, _, _), player2, Value) :-
    sum_score(Board1, x, Player11Score),   
    sum_score(Board1, o, Player12Score),
    sum_score(Board2, x, Player21Score),
    sum_score(Board2, o, Player22Score), 
    SafeDenominator is max(abs(Player11Score - Player12Score), 1),  
    Value is (Player21Score + Player22Score) / SafeDenominator.

% valid_moves(+GameState, -ListOfMoves)
% Finds all valid positions where a player can place a symbol.
valid_moves(game_state(ListR, ListC, Board, _, _, _, _, _), ListOfMoves) :-
    find_empty_positions(Board, ListR, ListC, ListOfMoves).

% find_empty_positions(+Board, +RowMapping, +ColMapping, -ListOfMoves)
% Iterates over all positions in the board and collects positions where the cell is 'z'.
find_empty_positions(Board, RowMapping, ColMapping, ListOfMoves) :-
    findall([RowLetter, ColNumber],
        (   nth1(RowIndex, Board, Row),             
            nth1(ColIndex, Row, Cell),             
            Cell = z,                              
            nth1(RowIndex, RowMapping, RowLetter), 
            nth1(ColIndex, ColMapping, ColNumber)  
        ),
        ListOfMoves).

