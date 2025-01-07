:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('game.pl').

board1([
    [z,o,o,o,o,x],
    [x,o,o,o,x,o],
    [x,o,o,o,x,o],
    [x,x,x,x,x,x],
    [x,x,o,x,x,o],
    [x,o,x,o,o,x]
]).

list_r1([a, b, c, d, e, f]).
list_c1([1, 2, 3, 4, 5, 6]).

board2([
    [o,x,o,o,o,x],
    [o,x,o,o,o,x],
    [x,x,o,x,o,x],
    [x,x,x,x,x,x],
    [o,x,x,o,x,o],
    [o,z,x,o,o,o]
]).

list_r2([c, b, e, d, f, a]).  
list_c2([4, 1, 6, 2, 3, 5]).  

% Test cases
run_tests :- 
    % Test 1: Both boards are completed, and we expect a winner
    list_r1(ListR1), list_c1(ListC1),
    list_r2(ListR2), list_c2(ListC2),
    board1(Board1), board2(Board2),

    GameState = game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, 2, o),
    display_game(GameState),
    choose_move(GameState, Level, Row-Col),
    move(GameState, Row-Col, NewGameState),
    game_over(NewGameState), !,
    display_game(NewGameState),
    close_game(NewGameState, _Winner),
    write('Goodbye!'), nl.

% Run tests automatically when the file is loaded
:- initialization(run_tests).



