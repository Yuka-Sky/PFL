:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- ensure_loaded('evaluate.pl').

% choose_move_human(+Board, +Player, (-InitRow)-(-InitCol), (-FinalRow)-(-FinalCol), -Type)
% Receives movement inputs until it is a valid input/move.
choose_move_human(Board, Player, Row-Col):-
    repeat,
    length(Board, Size),
    write('Choose movement: (e.g. \'a2.\')'), nl,
    read_move(Size, Row-Col),
    (validate_move(Board, Row-Col);
    write('Invalid movement. Try again:'), nl, fail).

% choose_move_comp
% Chooses a move for the computer based on the current game state and its level.
choose_move_comp(1, ListR, ListC, Board, Player, Symbol, Row-Col):-
    valid_moves(game_state(ListR, ListC, Board, _, _, _, _, _), ValidMoves),
    random_member([RowLetter, ColNumber], ValidMoves),
    nth1(RowIndex, ListR, RowLetter),
    nth1(ColIndex, ListC, ColNumber),
    Row = RowIndex, Col = ColIndex,
    write('Player '), write(Player), write(' marked the '), write(Symbol), write(' on '),
    write(RowLetter), write(ColNumber), nl, nl.


choose_move_comp(2, ListR, ListC, Board, Player, Symbol, Row-Col):-
/*    valid_moves(Board, Player, ValidMoves),    
*    % TO BE DONE
*    write('Player '), write(Player), write(' marked the '), write(Symbol), write(' on '),
*    index_to_notation(InitRow-InitCol, InitNotation),
*/   write('To be done'), nl, nl.


% choose_move(+Board, +Player, (-Row)-(-Col))
% Depending on game mode and player, chooses move as computer or human.
choose_move(game_state(ListR, ListC, Board1, _, _, _, _, _), Level, Row-Col) :-
    level_0(Level, _, _, Board1, Row-Col);
    level_1(Level, ListR, ListC, Board1, Row-Col);
    level_2(Level, ListR, ListC, Board1, Row-Col).

level_0(0, _, _, Board1, Row-Col) :-
    choose_move_human(Board1, _, Row-Col).

level_1(1, ListR, ListC, Board1, Row-Col) :-
    choose_move_comp(1, ListR, ListC, Board1, Player, _, Row-Col).

level_2(2, ListR, ListC, Board1, Row-Col) :-
    choose_move_comp(2, ListR, ListC, Board1, Player, _, Row-Col).

% validate_move(+Board, (+Row)-(+Col))   
% Given a choice of movement, is true when the move is valid, false otherwise.
validate_move(Board, Row-Col):-
    length(Board, Size),
    between(1, Size, Row),
    between(1, Size, Col),
    get_element(Board, Elem, Row, Col),
    Elem = z.

% get_space(+Board, -Elem, +RowIndex, +ColIndex)
% Gets a element in a given position on the board.
get_element(Board, Elem, RowIndex, ColIndex):-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, Elem).

% replace_elem(+List, +Index, +NewValue, -Result)
% Replaces element from a list in the given Index.
replace_elem(List, Index, NewValue, Result) :-
    nth1(Index, List, _, Rest),
    nth1(Index, Result, NewValue, Rest).

find_index(ListR1, ListC1, ListR2, ListC2, Row-Col, IndexRow1, IndexCol1, IndexRow2, IndexCol2) :-
    RowCode is Row + 96,       
    char_code(RowChar, RowCode),
    nth1(IndexRow1, ListR1, RowChar),
    nth1(IndexRow2, ListR2, RowChar),
    nth1(IndexCol1, ListC1, Col),
    nth1(IndexCol2, ListC2, Col).

move(game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, Player, Symbol), Row-Col, game_state(ListR1, ListC1, NewBoard1, ListR2, ListC2, NewBoard2, _, _)):-
    find_index(ListR1, ListC1, ListR2, ListC2, Row-Col, IndexRow1, IndexCol1,  IndexRow2, IndexCol2),
    nth1(IndexRow1, Board1, RowList1), 
    replace_elem(RowList1, IndexCol1, Symbol, NewRowList1),
    replace_elem(Board1, IndexRow1, NewRowList1, NewBoard1),
    nth1(IndexRow2, Board2, RowList2),
    replace_elem(RowList2, IndexCol2, Symbol, NewRowList2),
    replace_elem(Board2, IndexRow2, NewRowList2, NewBoard2).