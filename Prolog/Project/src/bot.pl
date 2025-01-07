% is_valid_move(+GameState, +Move, +Symbol)
% Verifies if a given move is valid by checking adjacency or diagonals to a player's existing symbols.
is_valid_move(state(PlayerGrid, OpponentGrid, GameBoard, Symbol), Move, Symbol) :-
    execute_move(state(PlayerGrid, OpponentGrid, GameBoard, Symbol), Move, NewState),
    is_adjacent_or_diagonal(NewState, Move, Symbol).

% execute_move(+GameState, +Move, -UpdatedState)
% Simulates the effect of a move and updates the game state.
execute_move(state(PlayerGrid, OpponentGrid, GameBoard, Symbol), Move, 
             state(NewPlayerGrid, OpponentGrid, NewBoard, Symbol)) :-
    apply_move(state(PlayerGrid, OpponentGrid, GameBoard, Symbol), Move, 
               state(NewPlayerGrid, OpponentGrid, NewBoard, Symbol)).

% is_adjacent_or_diagonal(+GameState, +Move, +Symbol)
% Checks if the move places a symbol adjacent or diagonal to an already existing symbol.
is_adjacent_or_diagonal(state(_, OpponentGrid, GameBoard, _), Move, Symbol) :-
    move_to_coordinates(Move, GameBoard, X, Y),
    (
        adjacent_or_diagonal_position(X, Y, AdjX, AdjY),
        Symbol_at_position(GameBoard, AdjX, AdjY, Symbol)
    ).

% adjacent_or_diagonal_position(+X, +Y, -AdjX, -AdjY)
% Finds all positions adjacent or diagonal to the given coordinates.
adjacent_or_diagonal_position(X, Y, AdjX, AdjY) :- 
    member(DX, [-1, 0, 1]),
    member(DY, [-1, 0, 1]),
    (DX \= 0; DY \= 0),  % Ensure at least one offset
    AdjX is X + DX,
    AdjY is Y + DY.

% Symbol_at_position(+GameBoard, +X, +Y, ?Symbol)
% Retrieves the symbol at the specified board coordinates.
Symbol_at_position(GameBoard, X, Y, Symbol) :-
    nth0(X, GameBoard, Row),
    nth0(Y, Row, Symbol).

% make_best_move(+GameState, -OptimalMove)
% Determines the best move based on game state analysis.
make_best_move(state(PlayerGrid, OpponentGrid, GameBoard, Symbol), OptimalMove) :-
    find_valid_moves(GameBoard, Symbol, AllMoves),
    exclude(is_invalid(state(PlayerGrid, OpponentGrid, GameBoard, Symbol)), AllMoves, ValidMoves),
    score_moves(state(PlayerGrid, OpponentGrid, GameBoard, Symbol), ValidMoves, ScoredMoves),
    select_best_move(ScoredMoves, OptimalMove).

% find_valid_moves(+GameBoard, +Symbol, -ValidMoves)
% Retrieves all valid moves for the given game state.
find_valid_moves(GameBoard, Symbol, ValidMoves) :-
    bagof(Move, valid_move(GameBoard, Symbol, Move), ValidMoves).

% score_moves(+GameState, +Moves, -ScoredMoves)
% Assigns scores to each possible move based on game state evaluation.
score_moves(GameState, Moves, ScoredMoves) :-
    findall(Score-Move, 
            (member(Move, Moves),
             execute_move(GameState, Move, UpdatedState),
             evaluate_game_state(UpdatedState, Score)),
            ScoredMoves).

% evaluate_game_state(+GameState, -Score)
% Computes a heuristic score for the given game state.
evaluate_game_state(state(_, _, GameBoard, Symbol), Score) :-
    count_Symbol(GameBoard, Symbol, SymbolCount),
    Score is SymbolCount.  % Example heuristic: maximize Symbol count

% select_best_move(+ScoredMoves, -BestMove)
% Selects the move with the highest score from a list of scored moves.
select_best_move(ScoredMoves, BestMove) :-
    max_member(_-BestMove, ScoredMoves).

% Example fallback to block opponent's move
block_opponent(state(_, OpponentGrid, GameBoard, _), ValidMoves, BestMove) :-
    score_moves(state(OpponentGrid, _, GameBoard, _), ValidMoves, OpponentScores),
    min_member(_-BestMove, OpponentScores).
