:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('menu.pl').
:- ensure_loaded('IO.pl').
:- ensure_loaded('board.pl').
:- ensure_loaded('view.pl').
:- ensure_loaded('bot.pl').
:- ensure_loaded('gameover.pl').
:- ensure_loaded('evaluate.pl').
:- ensure_loaded('movement.pl').

% play/0
% predicate to start the game
play :-
    write('\33\[2J'),
    menu(_Mode, Size, Player1, Level1, Player2, Level2, Player),
    initial_state(Size, Player1, Player2, GameState),
    take_Level(Player, Level1, Level2, Level),
    game_cycle(Level, Level1, Level2, GameState-(Player, _Symbol)).   

% take_Level(+Player, +Level1, +Level2, -Level)
% Determines the level of the current player.
take_Level(1, Level1, _Level2, Level1).
take_Level(2, _Level1, Level2, Level2).

% game_cycle(+Level, +Level1, +Level2, +GameState-(+Player, +Symbol))
% Main game loop that represents each player's turn until the game ends.
game_cycle(_, _, _, GameState-(_Player, _Symbol)):-
    game_over(GameState), !,
    display_game(GameState),
    close_game(GameState, _Winner),
    write('Goodbye!'), nl.

game_cycle(Level, Level1, Level2, GameState-(Player, Symbol)):-
    GameState = game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, Player, Symbol),
    display_game(GameState),
    write('Player '), write(Player), write('\'s turn | '), 
    write('Current Symbol: '), write(Symbol), nl,
    choose_move(GameState, Level, Row-Col),
    move(GameState, Row-Col, NewGameState),
    write('Click enter to proceed: '),
    read_line(_),
    next_player((Player, Symbol), (NextPlayer, NextSymbol)),!,
    take_Level(NextPlayer, Level1, Level2, NextLevel),
    game_cycle(NextLevel, Level1, Level2, NewGameState-(NextPlayer, NextSymbol)).

% next_player(+CurrentPlayer-Symbol, -NextPlayer-Symbol)
% Cycles through players and their symbols for each turn.
next_player((1, x), (1, o)).
next_player((1, o), (2, x)).
next_player((2, x), (2, o)).
next_player((2, o), (1, x)).

% display_game(+GameState)
% Displays the current game board for both players.
% Uses RowList and ColList to dynamically label rows and columns.
display_game(game_state(RowList1, ColList1, Board1, RowList2, ColList2, Board2, _, _)) :-
    write('\33\[2J'),
    write('Standard Board Player1:'), nl,
    display_board(RowList1, ColList1, Board1), nl, nl,
    write('Shuffled Board Player2:'), nl,
    display_board(RowList2, ColList2, Board2), nl, nl.
