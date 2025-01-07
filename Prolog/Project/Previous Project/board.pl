:- use_module(library(between)).
:- use_module(library(lists)).

% initial_state/2
% Generates the initial game state based on the given size.
initial_state(Size, GameState) :-
    between(5, 10, Size),
    
    % Create the first line
    create_first_line(Size, FirstLine),
    
    % Create the second line
    create_second_line(Size, SecondLine),
    
    % Create the third line if Size > 5, else create a specific line for Size = 5
    ( Size > 5 -> 
        create_third_line(Size, ThirdLine);
        ThirdLine = [p1, o, o, o, p2]
    ),
    
    % Create middle lines filled with 'o' if Size > 6
    ( Size > 6 -> 
        create_middle_rows(Size, Size-6, MiddlePart);
        MiddlePart = []
    ),
    
    % Create the third last line
    ( Size > 5 -> 
        create_third_last_line(Size, ThirdLastLine);
        ThirdLastLine = [p1, o, o, o, p2]
    ),
    
    % Create the second last line
    create_second_last_line(Size, SecondLastLine),
    
    % Create the last line
    create_last_line(Size, LastLine),
    
    % Concatenate all parts to form the game state
    ( Size < 6 -> 
        GameState = [FirstLine, SecondLine, ThirdLine, SecondLastLine, LastLine];
      Size =:= 6 -> 
        GameState = [FirstLine, SecondLine, ThirdLine, ThirdLastLine, SecondLastLine, LastLine];
      Size > 6 -> 
        append([FirstLine, SecondLine, ThirdLine], MiddlePart, Temp1),
        append(Temp1, [ThirdLastLine, SecondLastLine, LastLine], GameState)
    ).

% create_first_line/2
% Creates the first line of the board.
create_first_line(Size, FirstLine) :-
    create_empty_row(Size-4, Rest),
    append([g2, p1, p1], Rest, Temp),
    append(Temp, [x], FirstLine).

% create_second_line/2
% Creates the second line of the board.
create_second_line(Size, SecondLine) :-
    SecondLine = [p1 | Rest],
    create_empty_row(Size-1, Rest).

% create_third_line/2
% Creates the third line of the board.
create_third_line(Size, ThirdLine) :-
    ThirdLine = [p1 | Rest],
    create_empty_row(Size-1, Rest).


% create_middle_rows/3
% Creates the middle rows of the board.
create_middle_rows(_, 0, []).
create_middle_rows(Size, NeededLines, [MiddleLine | Rest]) :-
    NeededLines > 0,
    NeededLines1 is NeededLines - 1,
    create_empty_row(Size, MiddleLine),
    create_middle_rows(Size, NeededLines1, Rest).


% create_third_last_line/2
% Creates the third last line of the board.
create_third_last_line(Size, ThirdLastLine) :-
    ( Size = 6 -> 
        ThirdLastLine = [o, o, o, o, o, p2];
        create_empty_row(Size-1, Rest),
        append(Rest, [p2], ThirdLastLine)
    ).

% create_second_last_line/2
% Creates the second last line of the board.
create_second_last_line(Size, SecondLastLine) :-
    Size1 is Size - 1,  
    create_empty_row(Size1, Rest),  
    append(Rest, [p2], SecondLastLine).  


% create_last_line/2
% Creates the last line of the board.
create_last_line(Size, LastLine) :-
    Size1 is Size - 4,  
    create_empty_row(Size1, Middle),  
    append([x | Middle], [p2, p2, g1], LastLine).  


% create_empty_row/2
% Creates a single row filled with 'o'.
create_empty_row(0, []).
create_empty_row(Size, [o | Rest]) :-
    Size > 0,
    Size1 is Size - 1,
    create_empty_row(Size1, Rest).

% create_empty_rows/3
% Creates multiple rows filled with 'o'.
create_empty_rows(0, _, []).
create_empty_rows(N, Size, [Row | Rest]) :-
    N > 0,
    N1 is N - 1,
    create_empty_row(Size, Row),
    create_empty_rows(N1, Size, Rest).