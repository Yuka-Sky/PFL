
% choose_move_human(+Board, +Player, (-InitRow)-(-InitCol), (-FinalRow)-(-FinalCol), -Type)
% Receives movement inputs until it is a valid input/move
choose_move_human(Board, Player, InitRow-InitCol, FinalRow-FinalCol, Type):-
    repeat,
    write('move or capture?'),nl,
    write('[1] move'),nl,
    write('[2] capture'),nl,
    read(Type),
    (Type = 1; Type = 2; write('Invalid option'), nl, fail),
    length(Board, Size),
    write('Choose movement: (e.g. \'a2b2.\')'), nl,
    read_move(Size, InitRow-InitCol, FinalRow-FinalCol),
    (validate_move(Board, Type, Player, InitRow-InitCol, FinalRow-FinalCol);
    write('Invalid movement. Try again:'),nl,fail).


% choose_move_human(+MidBoard, (-Row)-(-Col))
% Receives position inputs until it is a valid input/position to place back piece
choose_move_human(MidBoard, Row-Col):-
    repeat,
    write('Choose an empty position to place back the piece you captured: (e.g. \'b2.\')'),nl,
    length(MidBoard, Size),
    read_place_back(Size, Row-Col),
    (validate_move(MidBoard, Row-Col);
    write('Invalid position.'),nl,fail).


% choose_move(+Board, +Player, (-InitRow)-(-InitCol), (-FinalRow)-(-FinalCol), -Type)
% Depending on game mode and player, chooses move as computer or human
choose_move(Board, Player, InitRow-InitCol, FinalRow-FinalCol, Type):-
    game_mode(1),!,
    choose_move_human(Board, Player, InitRow-InitCol, FinalRow-FinalCol, Type).

choose_move(Board, 1, InitRow-InitCol, FinalRow-FinalCol, Type):-
    game_mode(2),!,
    choose_move_human(Board, 1, InitRow-InitCol, FinalRow-FinalCol, Type).

choose_move(Board, 2, InitRow-InitCol, FinalRow-FinalCol, Type):-
    game_mode(2),!,
    comp_level(comp, Level),
    choose_move_comp(Level, Board, 2, InitRow-InitCol, FinalRow-FinalCol, Type).

choose_move(Board, 1, InitRow-InitCol, FinalRow-FinalCol, Type):-
    game_mode(3),
    comp_level(comp1, Level),!,
    choose_move_comp(Level, Board, 1, InitRow-InitCol, FinalRow-FinalCol, Type).

choose_move(Board, 2, InitRow-InitCol, FinalRow-FinalCol, Type):-
    game_mode(3),
    comp_level(comp2, Level),!,
    choose_move_comp(Level, Board, 2, InitRow-InitCol, FinalRow-FinalCol, Type).


% choose_move(+Player, +MidBoard, (-Row)-(-Col))
% Depending on game mode and player, chooses position to place piece back as computer or human
choose_move(_Player, MidBoard, Row-Col):-
    game_mode(1),!,
    choose_move_human(MidBoard, Row-Col).

choose_move(1, MidBoard, Row-Col):-
    game_mode(2),!,
    choose_move_human(MidBoard, Row-Col).

choose_move(2, MidBoard, Row-Col):-
    game_mode(2),!,
    comp_level(comp, Level),
    choose_move_comp(Level, MidBoard, 2, Row-Col).

choose_move(1, MidBoard, Row-Col):-
    game_mode(3),!,
    comp_level(comp1, Level),
    choose_move_comp(Level, MidBoard, 1, Row-Col).

choose_move(2, MidBoard, Row-Col):-
    game_mode(3),!,
    comp_level(comp2, Level),
    choose_move_comp(Level, MidBoard, 2, Row-Col).


% validate_move(+Board, +Type, +Player, (+InitRow)-(+InitCol), (+FinalRow)-(+FinalCol))   
% Given a move, is true when the move is valid, false otherwise
validate_move(Board, 1, 1, InitRow-InitCol, FinalRow-FinalCol):-
    length(Board, Size),
    between(1,Size,InitRow),
    between(1,Size,InitCol),
    between(1,Size,FinalRow),
    between(1,Size,FinalCol),
    get_piece(Board, InitPiece, InitRow, InitCol),
    InitPiece = p1,
    (FinalCol =:= InitCol, FinalRow =:= (InitRow + 1);
     FinalCol =:= (InitCol + 1), FinalRow =:= InitRow),
    get_piece(Board, FinalPiece, FinalRow, FinalCol),
    (FinalPiece = o; FinalPiece = g1).
    
validate_move(Board, 1, 2, InitRow-InitCol, FinalRow-FinalCol):-
    length(Board, Size),
    between(1,Size,InitRow),
    between(1,Size,InitCol),
    between(1,Size,FinalRow),
    between(1,Size,FinalCol),
    get_piece(Board, InitPiece, InitRow, InitCol),
    InitPiece = p2,
    (FinalCol =:= InitCol, FinalRow =:= (InitRow - 1);
     FinalCol =:= (InitCol - 1), FinalRow =:= InitRow),
    get_piece(Board, FinalPiece, FinalRow, FinalCol),
    (FinalPiece = o; FinalPiece = g2).

validate_move(Board, 2, 1, InitRow-InitCol, FinalRow-FinalCol):-
    length(Board, Size),
    between(1,Size,InitRow),
    between(1,Size,InitCol),
    between(1,Size,FinalRow),
    between(1,Size,FinalCol),
    get_piece(Board, InitPiece, InitRow, InitCol),
    InitPiece = p1,
    (FinalCol =:= (InitCol - 1), FinalRow =:= (InitRow - 1);
     FinalCol =:= (InitCol - 1), FinalRow =:= (InitRow + 1);
     FinalCol =:= (InitCol + 1), FinalRow =:= (InitRow - 1);
     FinalCol =:= (InitCol + 1), FinalRow =:= (InitRow + 1)),
    get_piece(Board, FinalPiece, FinalRow, FinalCol),
    FinalPiece = p2.

validate_move(Board, 2, 2, InitRow-InitCol, FinalRow-FinalCol):-
    length(Board, Size),
    between(1,Size,InitRow),
    between(1,Size,InitCol),
    between(1,Size,FinalRow),
    between(1,Size,FinalCol),
    get_piece(Board, InitPiece, InitRow, InitCol),
    InitPiece = p2,
    (FinalCol =:= (InitCol - 1), FinalRow =:= (InitRow - 1);
     FinalCol =:= (InitCol - 1), FinalRow =:= (InitRow + 1);
     FinalCol =:= (InitCol + 1), FinalRow =:= (InitRow - 1);
     FinalCol =:= (InitCol + 1), FinalRow =:= (InitRow + 1)),
    get_piece(Board, FinalPiece, FinalRow, FinalCol),
    FinalPiece = p1.


% validate_move(+Board, (+Row)-(+Col))
% Given a position to place piece back, is true when the position is valid, false otherwise
validate_move(Board, Row-Col):-
    length(Board, Size),
    between(1,Size,Row),
    between(1,Size,Col),
    get_piece(Board, Piece, Row, Col),
    Piece = o.

% move(+Board, (+InitRow)-(+InitCol), (+FinalRow)-(+FinalCol), -NewBoard)
% Moves piece from a board generating a new board
move(Board, InitRow-InitCol, FinalRow-FinalCol, NewBoard):-
    get_piece(Board, InitPiece, InitRow, InitCol),
    nth1(InitRow, Board, RowList1),
    replace_elem(RowList1, InitCol, o, NewRowList1),
    replace_elem(Board,InitRow,NewRowList1,MidBoard),
    nth1(FinalRow, MidBoard, RowList2),
    replace_elem(RowList2, FinalCol, InitPiece, NewRowList2),
    replace_elem(MidBoard,FinalRow,NewRowList2,NewBoard).


% move(+Board, +Player, (+Row)-(+Col), -NewBoard)
% Places back captured piece based on the player
move(Board, 1, Row-Col, NewBoard):-
    nth1(Row, Board, RowList),
    replace_elem(RowList, Col, p2, NewRowList),
    replace_elem(Board,Row,NewRowList,NewBoard).

move(Board, 2, Row-Col, NewBoard):-
    nth1(Row, Board, RowList),
    replace_elem(RowList, Col, p1, NewRowList),
    replace_elem(Board,Row,NewRowList,NewBoard).


% place_back(+Type, +Player, +MidBoard, -NewBoard)
% Calls predicates to place back a piece when a piece is captured on the last move
place_back(1, _Player, Board, Board).
place_back(2, Player, MidBoard, NewBoard):-
    display_game(MidBoard),nl,
    choose_move(Player, MidBoard, Row-Col),
    move(MidBoard, Player, Row-Col, NewBoard).


% valid_moves(Board, Player, ValidMoves)
% Generates a list of all valid moves for a player on a given board
valid_moves(Board, Player, ValidMoves):-
findall(InitRowOpt-InitColOpt-FinalRowOpt-FinalColOpt-TypeOpt, validate_move(Board, TypeOpt, Player, InitRowOpt-InitColOpt, FinalRowOpt-FinalColOpt), ValidMoves).
