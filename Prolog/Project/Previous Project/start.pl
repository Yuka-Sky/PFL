% Claustro Game

:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('menu.pl').
:- ensure_loaded('IO.pl').
:- ensure_loaded('movement.pl').
:- ensure_loaded('computer.pl').
:- ensure_loaded('board.pl').
:- ensure_loaded('view.pl').
:- ensure_loaded('gamelogic.pl').
:- ensure_loaded('helper.pl').


% play/0
% predicate to start the game
play :-
    reset_database,
    menu(Mode, Size, Player),
    asserta(game_mode(Mode)),
    initial_state(Size, GameState),!,
    game_cycle(GameState-Player).                   