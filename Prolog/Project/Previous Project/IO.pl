:- use_module(library(between)).
:- use_module(library(lists)).


% read_option(-Opt, +Max)
% Reads input until it is valid (between 1 and Max) and stores it in Opt
read_option(Opt, Max):-
    repeat,
    read(Opt),(
    integer(Opt), Opt >= 1, Opt =< Max ;
    write('Invalid input. Please enter a valid option between 1 and '), write(Max), nl, fail).


% read_move(+Size, (-InitRow)-(-InitCol), (-FinalRow)-(-FinalCol))
% Reads movement input and checks if it is valid (e.g. a2b2)
read_move(Size, InitRow-InitCol, FinalRow-FinalCol):-
    MaxCharCode is 97+Size,
    MaxNumberCode is 49+Size,
    read(Input),
    \+ number(Input),
    atom_codes(Input, CharList),
    length(CharList,4),
    nth1(1, CharList, InitColCode),
    between(97,MaxCharCode, InitColCode),
    nth1(2, CharList, InitRowCode),
    between(49,MaxNumberCode, InitRowCode),
    nth1(3, CharList, FinalColCode),
    between(97,MaxCharCode, FinalColCode),
    nth1(4, CharList, FinalRowCode),
    between(49,MaxNumberCode, FinalRowCode),
    InitCol is (InitColCode - 96),
    InitRow is (InitRowCode - 48),
    FinalCol is (FinalColCode - 96),
    FinalRow is (FinalRowCode - 48).
    

% read_place_back(+Size, (-Row)-(-Col))
% Reads placement input and checks if it is valid (e.g. a2)
read_place_back(Size, Row-Col):-
    MaxCharCode is 97+Size,
    MaxNumberCode is 49+Size,
    read(Input),
    \+ number(Input),
    atom_codes(Input, CharList),
    length(CharList,2),
    nth1(1, CharList, ColCode),
    between(97,MaxCharCode, ColCode),
    nth1(2, CharList, RowCode),
    between(49,MaxNumberCode, RowCode),
    Col is (ColCode - 96),
    Row is (RowCode - 48).    


% index_to_notation((+Row)-(+Col), -Notation)
% Gives the notation for a given position in the board
index_to_notation(Row-Col, Notation):-
    ColCode is Col + 96,
    RowCode is Row + 48,
    char_code(ColChar, ColCode),
    char_code(RowChar, RowCode),
    atom_concat(ColChar, RowChar, Notation).

  