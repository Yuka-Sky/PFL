:- ensure_loaded('score.pl').

% game_over(+GameState, -Winner)
% Checks if the current game state is final and determines the winner based on points.
game_over(game_state(_, _, Board1, _, _, Board2, _, _)):-
    board_completed(Board1),
    board_completed(Board2).

close_game(game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, Player, Symbol), Winner):-
    calculate_points(game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, Player, Symbol), Winner),
    print_winner(Winner).

% calculate_points(+Board1, +Board2, -Winner)
% Calculates the scores for both players and determines the winner.
calculate_points(game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, Player, Symbol), Winner) :-
    display_game(game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, Player, Symbol)),
    calculate_board_points(Board1, ScoreX1, ScoreO1),
    ScoreB1 is ScoreX1 + ScoreO1,
    calculate_board_points(Board2, ScoreX2, ScoreO2),
    ScoreB2 is ScoreX2 + ScoreO2,
    print_results(ScoreB1, ScoreB2),
    determine_winner(ScoreB1, ScoreB2, Winner).

% determine_winner(+ScoreB1, +ScoreB2, -Winner)
% Helper predicate to handle winner determination logic.
determine_winner(ScoreB1, ScoreB2, Winner) :-
    score_comparison(ScoreB1, ScoreB2, Winner).

% score_comparison(+ScoreB1, +ScoreB2, -Winner)
% Handles the comparison logic for determining the winner.
score_comparison(ScoreB1, ScoreB2, player1) :- ScoreB1 < ScoreB2.
score_comparison(ScoreB1, ScoreB2, player2) :- ScoreB1 > ScoreB2.
score_comparison(ScoreB1, ScoreB2, draw) :- ScoreB1 =:= ScoreB2.

% calculate_board_points(+Board, -ScoreX, -ScoreO)
% Calculates the individual scores for 'x' and 'o' on a given board.
calculate_board_points(Board, ScoreX, ScoreO):-
    sum_score(Board, x, ScoreX),
    sum_score(Board, o, ScoreO).

% print_results(+ScoreB1, +ScoreB2)
% Prints the final scores of both players.
print_results(ScoreB1, ScoreB2):-
    write('End of game! Calculating points...'), nl,
    write('Score Board1 (Player1): '), write(ScoreB1), nl,
    write('Score Board2 (Player2): '), write(ScoreB2), nl.

% print_winner(+Winner)
% Prints a winning message based on the winner.
print_winner(player1) :-
    write('Congratulations! Player 1 is the winner!'), nl.
print_winner(player2) :-
    write('Congratulations! Player 2 is the winner!'), nl.
print_winner(draw) :-
    write('It\'s a draw! Well played, both players!'), nl.

% row_completed(+Row)
% Checks if a row is completed (all elements aren't 'z').
row_completed([]).
row_completed([H|T]):-
    H \= z,
    row_completed(T).

% all_rows_completed(+Rows)
% Checks if all rows in the board are completed.
all_rows_completed([]).
all_rows_completed([H|T]):-
    row_completed(H),
    all_rows_completed(T).

% board_completed(+Board)
% Checks if the entire board is completed.
board_completed(Board):-
    all_rows_completed(Board).
