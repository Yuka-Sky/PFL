
%comp_level(+Computer,-Level)
%Computer level
:- dynamic comp_level/2.

% choose_move_comp(+Level, +Board, +Player, (-InitRow)-(-InitCol), (-FinalRow)-(-FinalCol), -Type)
% Chooses a move for the computer based on the current game state and the level of the computer
choose_move_comp(1, Board, Player, InitRow-InitCol, FinalRow-FinalCol, Type):-
    valid_moves(Board, Player, ValidMoves),
    random_member(InitRow-InitCol-FinalRow-FinalCol-Type, ValidMoves),
    write('Player '), write(Player),
    write(' moved piece from '),
    index_to_notation(InitRow-InitCol, InitNotation),
    index_to_notation(FinalRow-FinalCol, FinalNotation),
    write(InitNotation), write(' to '), write(FinalNotation),nl,nl.

choose_move_comp(2, Board, Player, InitRow-InitCol, FinalRow-FinalCol, Type):-
    valid_moves(Board, Player, ValidMoves),    
    maplist(apply_move(Board), ValidMoves, NewBoards),
    maplist(apply_value(Player), NewBoards, ValuesList),
    max_member(Max, ValuesList),
    bagof(Index, nth1(Index, ValuesList, Max), IndexList),
    random_member(RandomIndex, IndexList),
    nth1(RandomIndex, ValidMoves, InitRow-InitCol-FinalRow-FinalCol-Type),
    write('Player '), write(Player),
    write(' moved piece from '),
    index_to_notation(InitRow-InitCol, InitNotation),
    index_to_notation(FinalRow-FinalCol, FinalNotation),
    write(InitNotation), write(' to '), write(FinalNotation),nl,nl.
   
% choose_move_comp(+Level, +MidBoard, +Player, (-Row)-(-Col))
% Chooses a position to place back captured piece for the computer based on the current game state and the level of the computer   
choose_move_comp(1, MidBoard, Player, Row-Col):-
    bagof(RowOpt-ColOpt, validate_move(MidBoard, RowOpt-ColOpt), ValidMoves),
    random_member(Row-Col, ValidMoves),
    write('Player '), write(Player),
    write(' placed captured piece on '),
    index_to_notation(Row-Col, Notation),
    write(Notation),nl,nl.

choose_move_comp(2, MidBoard, Player, Row-Col):-
    bagof(RowOpt-ColOpt, validate_move(MidBoard, RowOpt-ColOpt), ValidMoves),
    maplist(move(MidBoard, Player), ValidMoves, NewBoards),
    maplist(apply_value(Player), NewBoards, ValuesList),
    max_member(Max, ValuesList),
    bagof(Index, nth1(Index, ValuesList, Max), IndexList),
    random_member(RandomIndex, IndexList),
    nth1(RandomIndex, ValidMoves, Row-Col),
    write('Player '), write(Player),
    write(' placed captured piece on '),
    index_to_notation(Row-Col, Notation),
    write(Notation),nl,nl.


% value_piece(+Board, (+RowIndex)-(+ColIndex), +Player, -Value)
% determines the value of a piece in a position for a player based on the distance to the piece's objective
value_piece(Board, RowIndex-ColIndex, 1, Value):-
    get_piece(Board, Piece, RowIndex, ColIndex),
    Piece = p1, !,
    length(Board, Size),
    distance(RowIndex-ColIndex, Size-Size, Dist),
    (Dist = 0, Value = 1000;
     DistMax is Size * 2 - 3,
    B is (- (DistMax * DistMax) - 100)/DistMax,
    Value is (Dist * Dist) + (B * Dist) + 100).
    
value_piece(Board, RowIndex-ColIndex, 1, Value):-
    get_piece(Board, Piece, RowIndex, ColIndex),
    Piece = p2, !,
    length(Board, Size),
    distance(RowIndex-ColIndex, 1-1, Dist),
    (Dist = 0, Value = -1000;
    DistMax is Size * 2 - 3,
    B is (- (DistMax * DistMax) - 100)/DistMax,
    Value is -(Dist * Dist) - (B * Dist) - 100).

value_piece(Board, RowIndex-ColIndex, 2, Value):-
    get_piece(Board, Piece, RowIndex, ColIndex),
    Piece = p2, !,
    length(Board, Size),
    distance(RowIndex-ColIndex, 1-1, Dist),
    (Dist = 0, Value = 1000;
    DistMax is Size * 2 - 3,
    B is (- (DistMax * DistMax) - 100)/DistMax,
    Value is (Dist * Dist) + (B * Dist) + 100).

value_piece(Board, RowIndex-ColIndex, 2, Value):-
    get_piece(Board, Piece, RowIndex, ColIndex),
    Piece = p1, !,
    length(Board, Size),
    distance(RowIndex-ColIndex, Size-Size, Dist),
    (Dist = 0, Value = -1000;
    DistMax is Size * 2 - 3,
    B is (- (DistMax * DistMax) - 100)/DistMax,
    Value is -(Dist * Dist) - (B * Dist) - 100).
    
value_piece(_Board, _RowIndex-_ColIndex, _Player, 0).


% value(+GameState, +Player, -Value)
% Determines the total value of a board for a given player 
value(GameState, Player, Value):-
    findall(DistValue, piece_dist_value(GameState, Player, DistValue), ValueList),
    sumlist(ValueList, Value).
    
% piece_dist_value(+GameState, +Player, -Value)
% Generates the values for each piece in the board for a given player
piece_dist_value(GameState, Player, Value):-    
    length(GameState, Size),
    between(1, Size, RowIndex),
    between(1, Size,ColIndex),    
    value_piece(GameState, RowIndex-ColIndex, Player, Value).






    