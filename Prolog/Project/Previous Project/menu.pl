:- ensure_loaded('IO.pl').
:- ensure_loaded('computer.pl').

%title/0
%Writes the title of the game
title :- write('   ___ _                 _              '),nl,
         write('  / __\\ | __ _ _   _ ___| |_ _ __ ___   '),nl,
         write(' / /  | |/ _` | | | / __| __| \'__/ _ \\  '),nl,
         write('/ /___| | (_| | |_| \\__ \\ |_| | | (_) | '),nl,
         write('\\____/|_|\\__,_|\\__,_|___/\\__|_|  \\___/  '),nl,nl.

%game_mode_menu(-Mode)
%Writes game mode options and reads option chosen
game_mode_menu(Mode) :-
        write('Select a game mode:'),nl,
        write('[1] Player vs Player'),nl,
        write('[2] Player vs Computer'),nl,
        write('[3] Computer vs Computer'),nl,
        read_option(Mode, 3).

% set_computer_level(+Mode)
% Reads and sets the computer level (if necessary) for each game mode
set_computer_level(1).
set_computer_level(2):-
        write('Choose level for computer:'),nl,
        write('[1] level 1 - Random moves'),nl,
        write('[2] level 2 - Greedy moves'),nl,
        read_option(CompLevel, 2),
        assert(comp_level(comp, CompLevel)).
set_computer_level(3):-
        write('Choose level for computer 1:'),nl,
        write('[1] level 1 - Random moves'),nl,
        write('[2] level 2 - Greedy moves'),nl,
        read_option(Comp1Level, 2),
        assert(comp_level(comp1, Comp1Level)),
        write(Comp1Level),nl,
        write('Choose level for computer 2:'),nl,
        write('[1] level 1 - Random moves'),nl,
        write('[2] level 2 - Greedy moves'),nl,
        read_option(Comp2Level, 2),
        assert(comp_level(comp2, Comp2Level)).

% board_size(-Size)
% Displays the board size options and reads choice
board_size(Size):-
        write('Choose Board Size:'),nl,
        write('[1] 5x5'),nl,
        write('[2] 6x6'),nl,
        write('[3] 7x7'),nl,
        write('[4] 8x8'),nl,
        write('[5] 9x9'),nl,
        read_option(Input, 5),
        Size is Input +4.

% player_first_move(-Player, +GameMode)
% Displays starting player options and reads result
player_first_move(Player, 1):-
        write('Choose who starts:'),nl,
        write('[1] Player 1'),nl,
        write('[2] Player 2'),nl,
        read_option(Player, 2).

player_first_move(Player, 2):-
        write('Choose who starts:'),nl,
        write('[1] Player (Player 1)'),nl,
        write('[2] Computer (Player 2)'),nl,
        read_option(Player, 2).

player_first_move(Player, 3):-
        write('Choose who starts:'),nl,
        write('[1] Computer 1 (Player 1)'),nl,
        write('[2] Computer 2 (Player 2)'),nl,
        read_option(Player, 2).

% menu(-Mode, -Size, -Player)
% Displays whole menu and reads the user's answers
menu(Mode, Size, Player):-
        title,
        game_mode_menu(Mode),
        set_computer_level(Mode),
        board_size(Size),
        player_first_move(Player, Mode).
        