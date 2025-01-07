list_size([], 0).
list_size([_|T], Size):-
    list_size(T, Size1),
    Size is Size1 + 1.