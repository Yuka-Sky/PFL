

%game_mode(+Mode)
%Dynamic predicate to "store" the current game mode 
:- dynamic game_mode/1.


% game_cycle(GameState-Player)
% main game cycle that represents each turn
game_cycle(GameState-_Player):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    congratulate(Winner).

game_cycle(GameState-Player):-
    write('Player '), write(Player), write('\'s turn'), nl,nl,
    display_game(GameState),
    choose_move(GameState, Player, InitRow-InitCol, FinalRow-FinalCol, Type),
    move(GameState, InitRow-InitCol, FinalRow-FinalCol, MidGameState),
    place_back(Type, Player, MidGameState, NewGameState),
    next_player(Player, NextPlayer),!, 
    game_cycle(NewGameState-NextPlayer).


% next_player(+CurrentPlayer, -NextPlayer).
% Given a player number gets the next player's number
next_player(1,2).
next_player(2,1). 


% game_over(+GameState, +Player)
% Checks if the current game state is final
game_over(GameState, 1):-
    length(GameState, Size),
    get_piece(GameState, Piece, Size, Size),
    Piece = p1.

game_over(GameState, 2):-
    get_piece(GameState, Piece, 1, 1),
    Piece = p2.


% congratulate(+Winner)
% Writes congratulatory message for winner
congratulate(Winner):-
    write('Player '), write(Winner), write(' won!'),nl.
