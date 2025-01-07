


% replace_elem(+List, +Index, +NewValue, -Result)
% Replaces element from a list in the given Index
replace_elem(List, Index, NewValue, Result) :-
    nth1(Index, List, _, Rest),
    nth1(Index, Result, NewValue, Rest).


% reset_database/0
% retracts clauses from dynamic predicates 
reset_database:-
    retractall(game_mode(_)),
    retractall(comp_level(_,_)).
        

% get_piece(+Board, -Piece, +RowIndex, +ColIndex)
% gets a piece in a given position on the board
get_piece(Board, Piece, RowIndex, ColIndex):-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, Piece).


% apply_value(+Player, +Board, -Value)
% Helper function to make it possible for value to be used with maplist
apply_value(Player, Board, Value):-
    value(Board, Player, Value).

% apply_move(+Board, +(InitRow-InitCol-FinalRow-FinalCol-_Type), -NewBoard)
% Helper function to make it possible for move to be used with maplist
apply_move(Board, InitRow-InitCol-FinalRow-FinalCol-_Type, NewBoard) :-
    move(Board, InitRow-InitCol, FinalRow-FinalCol, NewBoard).


% distance((+RowIndex1)-(+ColIndex1), (+RowIndex2)-(+ColIndex2), -Dist)
% Calculates the distance between two positions on the board 
distance(RowIndex1-ColIndex1, RowIndex2-ColIndex2, Dist) :-
    Dist is abs(RowIndex1 - RowIndex2) + abs(ColIndex1 - ColIndex2).