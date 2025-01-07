% sum_score(+Board, +Symbol, -Score)
% Calculates the total score for a given board based on complete lines, squares, and diagonals of the specified symbol.
sum_score(Board, Symbol, Score) :-
    replace_z_with_space(Board, CleanedBoard),
    findall(1, horizontal_line(CleanedBoard, Symbol), HorizontalScore),
    findall(1, vertical_line(CleanedBoard, Symbol), VerticalScore),
    findall(1, square(CleanedBoard, Symbol), SquareScore),
    findall(1, diagonal_line(CleanedBoard, Symbol), DiagonalScore),
    length(HorizontalScore, HorScore),
    length(VerticalScore, VerScore),
    length(SquareScore, SqScore),
    length(DiagonalScore, DiaScore),
    Score is HorScore + VerScore + SqScore + DiaScore.

% replace_z_with_space(+Board, -CleanedBoard)
% Replaces all 'z' symbols in the board with spaces.
replace_z_with_space([], []).
replace_z_with_space([Row|Rest], [CleanedRow|CleanedRest]) :-
    maplist(replace_z, Row, CleanedRow),
    replace_z_with_space(Rest, CleanedRest).

% replace_z(+Element, -CleanedElement)
% Replaces 'z' with a space; leaves other elements unchanged.
replace_z(z, ' ').
replace_z(X, X) :- X \= z.

% horizontal_line(+Board, +Symbol)
% True if any row in the board contains a horizontal line of four matching symbols.
horizontal_line(Board, Symbol) :-
    member(Line, Board),
    sublist([Symbol, Symbol, Symbol, Symbol], Line).

% vertical_line(+Board, +Symbol)
% True if any column in the board contains a vertical line of four matching symbols.
vertical_line(Board, Symbol) :-
    transpose(Board, TransposedBoard),
    horizontal_line(TransposedBoard, Symbol).

% square(+Board, +Symbol)
% True if there is a 2x2 square of the same symbol in the board.
square(Board, Symbol) :-
    length(Board, Size),
    between(1, Size, Line),
    between(1, Size, Column),
    nth1(Line, Board, Line1),
    nth1(Column, Line1, Symbol),
    Line2Index is Line + 1,
    nth1(Line2Index, Board, Line2),
    Column2Index is Column + 1,
    nth1(Column, Line2, Symbol),
    nth1(Column2Index, Line1, Symbol),
    nth1(Column2Index, Line2, Symbol).

% diagonal_line(+Board, +Symbol)
% True if any diagonal of four matching symbols is present on the board.
diagonal_line(Board, Symbol) :-
    descending_diagonal(Board, Symbol);
    ascending_diagonal(Board, Symbol).

% descending_diagonal(+Board, +Symbol)
% True if there is a descending diagonal of four matching symbols.
descending_diagonal(Board, Symbol) :-
    length(Board, Size),
    Max is Size - 3,
    between(1, Max, Line),
    between(1, Max, Column),
    nth1(Line, Board, Line1),
    nth1(Column, Line1, Symbol),
    Line2 is Line + 1,
    Column2 is Column + 1,
    nth1(Line2, Board, Line2_),
    nth1(Column2, Line2_, Symbol),
    Line3 is Line + 2,
    Column3 is Column + 2,
    nth1(Line3, Board, Line3_),
    nth1(Column3, Line3_, Symbol),
    Line4 is Line + 3,
    Column4 is Column + 3,
    nth1(Line4, Board, Line4_),
    nth1(Column4, Line4_, Symbol).

% ascending_diagonal(+Board, +Symbol)
% True if there is an ascending diagonal of four matching symbols.
ascending_diagonal(Board, Symbol) :-
    length(Board, Size),
    Max is Size - 3,
    between(1, Max, Line),
    between(4, Size, Column),
    nth1(Line, Board, Line1),
    nth1(Column, Line1, Symbol),
    Line2 is Line + 1,
    Column2 is Column - 1,
    nth1(Line2, Board, Line2_),
    nth1(Column2, Line2_, Symbol),
    Line3 is Line + 2,
    Column3 is Column - 2,
    nth1(Line3, Board, Line3_),
    nth1(Column3, Line3_, Symbol),
    Line4 is Line + 3,
    Column4 is Column - 3,
    nth1(Line4, Board, Line4_),
    nth1(Column4, Line4_, Symbol).

% sublist(+SubList, +List)
% Auxiliar function.
sublist(SubList, List) :-
    append(_, Rest, List),
    append(SubList, _, Rest).
