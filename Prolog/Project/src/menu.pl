:- ensure_loaded('IO.pl').

% title/0
% Writes the title of the game.
title :- write('   ____          _      _  _       '), nl,
        write('  |  _ \\   ___  | |__  | |(_) _ ___'), nl,
        write('  | | | | / _ \\ |  _ \\ | | _ | /__ \\  '), nl,
        write('  | |_| || (_) || (_) || || || |  | |'), nl,
        write('  |____/  \\___/ |____/ |_||_||_|  |_|'), nl, nl.



% game_mode_menu(-Mode)
% Writes game mode options and reads option chosen.
game_mode_menu(Mode) :-
        write('Select a game mode:'), nl,
        write('[1] Player vs Player'), nl,
        write('[2] Player vs Computer'), nl,
        write('[3] Computer vs Computer'), nl,
        read_option(Mode, 3).

% set_computer_level(+Mode)
% Reads and sets the computer level (if necessary) for each game mode.
set_computer_level(1, Player1, Level1, Player2, Level2):-
        Player1 = player1,
        Level1 = 0,
        Player2 = player2,
        Level2 = 0.
set_computer_level(2, Player1, Level1, Player2, Level2):-
        write('Choose level for computer:'), nl,
        write('[1] level 1 - Random moves'), nl,
        write('[2] level 2 - Greedy moves'), nl,
        read_option(CompLevel, 2),
        Player1 = player1,
        Level1 = 0,
        Player2 = computer2,
        Level2 = CompLevel.
set_computer_level(3, Player1, Level1, Player2, Level2):-
        write('Choose level for computer 1:'), nl,
        write('[1] level 1 - Random moves'), nl,
        write('[2] level 2 - Greedy moves'), nl,
        read_option(Comp1Level, 2),
        write(Comp1Level), nl,
        write('Choose level for computer 2:'), nl,
        write('[1] level 1 - Random moves'), nl,
        write('[2] level 2 - Greedy moves'), nl,
        read_option(Comp2Level, 2),
        Player1 = computer1,
        Level1 = Comp1Level,
        Player2 = computer2,
        Level2 = Comp2Level.

% board_size(-Size)
% Displays the board size options and reads choice.
board_size(Size):-
        write('Choose Board Size:'), nl,
        write('[1] 6x6'), nl,
        write('[2] 8x8'), nl,
        read_option(Input, 2),
        Size is 2^Input +4.

% player_first_move(+GameMode, -Player, -Player1, -Player2)
% Displays starting player options and assigns Player1 and Player2 based on the choice.
player_first_move(1, Player):-  % Player vs Player
    write('Choose who starts:'), nl,
    write('[1] Player 1'), nl,
    write('[2] Player 2'), nl,
    read_option(Player, 2).

player_first_move(2, Player):-  % Player vs Computer
    write('Choose who starts:'), nl,
    write('[1] Player (Player 1)'), nl,
    write('[2] Computer (Player 2)'), nl,
    read_option(Player, 2).

player_first_move(3, Player):-  % Computer vs Computer
    write('Choose who starts:'), nl,
    write('[1] Computer 1 (Player 1)'), nl,
    write('[2] Computer 2 (Player 2)'), nl,
    read_option(Player, 2).
   
% menu(-Mode, -Size, -Player1, -Player2, -Player)
% Displays the entire menu and initializes all required variables.
menu(Mode, Size, Player1, Level1, Player2, Level2, Player):-
    title,
    game_mode_menu(Mode),
    board_size(Size),
    set_computer_level(Mode, Player1, Level1, Player2, Level2),
    player_first_move(Mode, Player).
