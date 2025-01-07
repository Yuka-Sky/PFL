:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

% initial_state(+Size, +[Player1, Player2], -game_state(Board1, Board2, CurrentPlayer, Symbol)
% Generates the initial game state based on the given game configuration (Size and Players).
initial_state(Size, Player1, Player2, game_state(ListR1, ListC1, Board1, ListR2, ListC2, Board2, _Player, x)):-
    create_RowColLists(Player1, Size, ListR1, ListC1, Board1),
    create_RowColLists(Player2, Size, ListR2, ListC2, Board2).

% create_RowColLists(+Type, +Size, -FinalRowList, -FinalColList, -Table)
% Generates the row and column labels, the board, and shuffle for player 2.
create_RowColLists(Type, Size, FinalRowList, FinalColList, Table):-
    generate_rowlist(Size, RowList),
    generate_collist(Size, ColList),
    generate_table(RowList, ColList, Table),
    process_player(Type, RowList, ColList, FinalRowList, FinalColList).

% generate_rowlist(+Size, -RowList)
% Generates a list of row labels based on the board size.
generate_rowlist(Size, RowList):-
    findall(Letter, (between(1, Size, N), char_code('a', A), LetterCode is A + N - 1, char_code(Letter, LetterCode)), RowList).

% generate_collist(+Size, -ColList)
% Generates a list of column labels based on the board size.
generate_collist(Size, ColList):-
    findall(N, between(1, Size, N), ColList).

% generate_table(+RowList, +ColList, -Table)
% Creates a board filled with 'z'.
generate_table([], _ColList, []).
generate_table([_ | RowRest], ColList, [Row | TableRest]) :-
    length(ColList, ColSize),
    create_empty_row(ColSize, Row),
    generate_table(RowRest, ColList, TableRest).

% create_empty_row(+Size, -Row)
% Creates a single row filled with 'z'.
create_empty_row(0, []).
create_empty_row(Size, [z | Rest]):-
    Size > 0,
    Size1 is Size - 1,
    create_empty_row(Size1, Rest).

% process_player(+Type, +RowList, +ColList, -FinalRowList, -FinalColList)
% Adjusts the row and column lists based on the player type.
process_player(player1, RowList, ColList, RowList, ColList).
process_player(player2, RowList, ColList, ShuffledRowList, ShuffledColList):-
    random_permutation(RowList, ShuffledRowList),
    random_permutation(ColList, ShuffledColList).
process_player(computer1, RowList, ColList, RowList, ColList).
process_player(computer2, RowList, ColList, ShuffledRowList, ShuffledColList):-
    random_permutation(RowList, ShuffledRowList),
    random_permutation(ColList, ShuffledColList).
