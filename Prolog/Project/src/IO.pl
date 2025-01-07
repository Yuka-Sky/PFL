:- use_module(library(between)).
:- use_module(library(lists)).

% read_option(-Opt, +Max)
% Reads input until it is valid (between 1 and Max) and stores it in Opt
read_option(Opt, Max):-
    repeat,
    read(Opt),(
    integer(Opt), Opt >= 1, Opt =< Max ;
    write('Invalid input. Please enter a valid option between 1 and '), write(Max), nl, fail).

% read_move(+Size, -Row-Col)
% Reads and parses the move input while ensuring it is within bounds.
read_move(Size, Row-Col) :-
    MaxCharCode is 97 + Size,
    MaxNumberCode is 49 + Size,
    read(Input),
    atom_chars(Input, Chars),
    valid_move_format(Chars, Size, Row, Col, MaxCharCode, MaxNumberCode).

% valid_move_format(+Chars, +Size, -Row, -Col, +MaxCharCode, +MaxNumberCode)
% Validates the input format and extracts Row and Col from the characters.
valid_move_format([RowChar, ColChar], Size, Row, Col, MaxCharCode, MaxNumberCode) :-
    char_code(RowChar, RowCode),
    char_code(ColChar, ColCode),
    between(97, MaxCharCode, RowCode),
    between(49, MaxNumberCode, ColCode),
    Row is (RowCode - 96),
    Col is (ColCode - 48).

valid_move_format(_, _, _, _, _, _) :-
    write('Invalid input format. Expected something like "a1".'), nl,
    fail.

