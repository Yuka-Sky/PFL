% display_board(+RowList, +ColList, +Board)
% Displays a single game board with dynamic row and column labels.
display_board(RowList, ColList, Board):-
    length(RowList, Length),
    display_header(ColList),
    display_middle_board(Board, Length, RowList).

% display_header(+ColList)
% Displays the header of the board with dynamic column labels.
display_header(ColList):-
    write('    '),
    display_column_labels(ColList), nl.

% display_column_labels(+ColList)
% Recursively displays the column labels from ColList.
display_column_labels([]).
display_column_labels([Col|Cols]):-
    write('   '), write(Col), write('  '),
    display_column_labels(Cols).

% display_middle_board(+Board, +Length, +RowList)
% Displays the rows of the board along with their respective row labels.
display_middle_board([], _, []) :- !.

display_middle_board([Row|[]], _, [RowLabel|[]]):-
    length(Row, Length),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    write(' '), write(RowLabel), write('  '),
    display_intersected_row(Row, "|", "|", "|"),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    display_lengthed_row(Length, "-", "|", "|", "|").

display_middle_board([Row|OtherRows], Length, [RowLabel|OtherRowLabels]):-
    length(Row, Length),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    write(' '), write(RowLabel), write('  '),
    display_intersected_row(Row, "|", "|", "|"),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    display_lengthed_row(Length, "-", "|", "|", "|"),
    display_middle_board(OtherRows, Length, OtherRowLabels).

% display_lengthed_row(+Length, +Spacing, +LeftSymbol, +MiddleSymbol, +RightSymbol)
% Displays a row of the board with a specific length and border symbols.
display_lengthed_row(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol):-
    write('    '),
    write_string(LeftSymbol),
    display_lengthed_row_aux(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol),
    write_string(RightSymbol),
    write('\n').

% display_lengthed_row_aux(+Length, +Spacing, +LeftSymbol, +MiddleSymbol, +RightSymbol)
% Recursively generates the row content for a specific length.
display_lengthed_row_aux(1, Spacing, _LeftSymbol, _MiddleSymbol, _RightSymbol):-
    display_line_elem(Spacing), !.

display_lengthed_row_aux(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol):-
    display_line_elem(Spacing),
    write_string(MiddleSymbol),
    NewLength is Length -1,
    display_lengthed_row_aux(NewLength, Spacing, LeftSymbol, MiddleSymbol, RightSymbol).

% display_intersected_row(+List, +LeftSymbol, +MiddleSymbol, +RightSymbol)
% Displays a row of intersected board elements with borders.
display_intersected_row(List, LeftSymbol, MiddleSymbol, RightSymbol):-
    write_string(LeftSymbol),
    display_intersected_row_aux(List, LeftSymbol, MiddleSymbol, RightSymbol),
    write_string(RightSymbol),
    write('\n').

% display_intersected_row_aux(+List, +LeftSymbol, +MiddleSymbol, +RightSymbol)
% Recursively displays intersected elements in the row.
display_intersected_row_aux([Elem|[]], _LeftSymbol, _MiddleSymbol, _RightSymbol):-
    display_info_elem(Elem).
display_intersected_row_aux([Elem|List], LeftSymbol, MiddleSymbol, RightSymbol):-
    display_info_elem(Elem),
    write_string(MiddleSymbol),
    display_intersected_row_aux(List, LeftSymbol, MiddleSymbol, RightSymbol).

% display_info_elem(+Elem)
% Displays a single board element with formatting based on its value.
display_info_elem(Elem):-
    (Elem == o, display_line_elem("o"));
    (Elem == x, display_line_elem("x"));
    (Elem == z, display_line_elem(" ")).

% display_line_elem(+Elem)
% Displays a single element with padding.
display_line_elem(Elem):-
    write('  '), write_string(Elem), write('  ').

% write_string(+String)
% Outputs a string to the console.
write_string(String):-
    format('~s',[String]).

% display_bottom(+Length)
% Displays the bottom border of the board based on the row length.
display_bottom(Length):- 
    display_lengthed_row(Length, "-", "-", "-", "-").