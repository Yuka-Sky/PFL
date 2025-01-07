% display_game/1
% Displays the game board by calling other display functions.
display_game([FirstRow|OtherRows]):-
    length(FirstRow, Length),
    display_header(Length),
    display_middle_board([FirstRow|OtherRows], Length, 1).

% display_header/1
% Displays the header with column labels.
display_header(Length) :-
    write('    '), 
    display_column_labels(Length, 0, 'a').

% display_column_labels/3
% Recursive predicate to display column labels.
display_column_labels(0, _, _) :- nl.
display_column_labels(Length, Index, Char) :-
    NextIndex is Index + 1,
    NextLength is Length - 1,
    write('   '), write(Char), write('  '),
    char_code(Char, CharCode),
    NextCharCode is CharCode + 1,
    char_code(NextChar, NextCharCode),
    display_column_labels(NextLength, NextIndex, NextChar).

% display_middle_board/3
% Recursive predicate to display rows of the board.
display_middle_board([Row|[]], _, LineNum):-
    length(Row, Length),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    display_row(Row, LineNum),
    display_intersected_row(Row, "|", "|", "|"),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    display_lengthed_row(Length, "-", "|", "|", "|").

display_middle_board([Row|OtherRows], Length, LineNum):-
    length(Row, Length),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    display_row(Row, LineNum),
    display_intersected_row(Row, "|", "|", "|"),
    display_lengthed_row(Length, " ", "|", "|", "|"),
    display_lengthed_row(Length, "-", "|", "|", "|"),
    NextLineNum is LineNum + 1,
    display_middle_board(OtherRows, Length, NextLineNum).

% display_row/2
% Displays a single row with line number.
display_row(_List, LineNum) :-
    format('~|~`0t~d~2+  ', [LineNum]). % Print line number with padding

% display_lengthed_row_no_space/5
% Helper function to display a lengthed row without initial space.
display_lengthed_row_no_space(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol):-
    write_string(LeftSymbol),
    display_lengthed_row_aux(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol),
    write_string(RightSymbol),
    write('\n').

% display_lengthed_row/5
% Helper function to display a lengthed row with initial space.
display_lengthed_row(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol):-
    write('    '),
    write_string(LeftSymbol),
    display_lengthed_row_aux(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol),
    write_string(RightSymbol),
    write('\n').

% display_lengthed_row_aux/5
% Recursive helper function to display a lengthed row.
display_lengthed_row_aux(1, Spacing, _LeftSymbol, _MiddleSymbol, _RightSymbol):-
    display_line_elem(Spacing).
display_lengthed_row_aux(Length, Spacing, LeftSymbol, MiddleSymbol, RightSymbol):-
    display_line_elem(Spacing),
    write_string(MiddleSymbol),
    NewLength is Length -1,
    display_lengthed_row_aux(NewLength, Spacing, LeftSymbol, MiddleSymbol, RightSymbol).

% display_intersected_row/4
% Displays the intersected row of the board.
display_intersected_row(List, LeftSymbol, MiddleSymbol, RightSymbol):-
    %write(''),
    write_string(LeftSymbol),
    display_intersected_row_aux(List, LeftSymbol, MiddleSymbol, RightSymbol),
    write_string(RightSymbol),
    write('\n').

% display_intersected_row_aux/4
% Recursive helper function to display intersected row elements.
display_intersected_row_aux([Elem|[]], _LeftSymbol, _MiddleSymbol, _RightSymbol):-
    display_info_elem(Elem).

display_intersected_row_aux([Elem|List], LeftSymbol, MiddleSymbol, RightSymbol):-
    display_info_elem(Elem),
    write_string(MiddleSymbol),
    display_intersected_row_aux(List, LeftSymbol, MiddleSymbol, RightSymbol).

% display_info_elem/1
% Displays a single information element on the board.
display_info_elem(Elem):-
    (Elem == g1, display_line_elem("g1"));
    (Elem == g2, display_line_elem("g2"));
    (Elem == p1, display_line_elem("p1"));
    (Elem == p2, display_line_elem("p2"));
    (Elem == x, display_line_elem("x"));
    (Elem == o, display_line_elem(" ")).

% display_bottom/1
% Displays the bottom border of the board.
display_bottom(Length):- 
    display_lengthed_row(Length, "-", "-", "-", "-").

% write_string/1
% Writes a string to the console.
write_string(String):-
    format('~s',[String]).

% display_line_elem/1
% Displays a single line element with padding.
display_line_elem(Elem):-
    % write(Elem).
    % write_string(Elem).
    length(Elem, StringLength),
    RemainToPad is 5 - StringLength,
    LeftPad is ( (RemainToPad // 2) + (RemainToPad mod 2)),
    RightPad is  RemainToPad // 2,
    write_padding(LeftPad),
    write_string(Elem),
    write_padding(RightPad).

% write_padding/1
% Recursive predicate to write padding spaces.
write_padding(0).
write_padding(Length):-
    write(' '),
    NewLength is Length - 1,
    write_padding(NewLength).

