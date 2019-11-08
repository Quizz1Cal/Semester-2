sumlist(Ns, Sum) :-
    sumlist(Ns, Sum, 0).

sumlist([], Sum, Sum).
sumlist([N|Ns], Sum, Sum0) :-
    Sum1 is N + Sum0,
    sumlist(Ns, Sum, Sum1).

tree(empty).
tree(node(Left,_,Right)) :-
    tree(Left), 
    tree(Right).

%Q2 tree_list/2(+Tree, ?Xs)
tree_list(empty, []).
tree_list(node(Left, X, Right), Xs) :-
    tree_list(Left, Ls),
    tree_list(Right, Rs),
    append(Ls, [X|Rs], Xs).

%Q3 tree_list2/2(?Tree, ?Xs)
% NOTE TO SELF: I HAD THE IDEA RIGHT ABOUT THE ACC, BUT WASNT FLEXIBLE THINKING

% accumulate: values right

%Q4 list_tree(?List, +Tree)
list_tree([], empty).
list_tree(Xs, node(L, V, R)) :-
    append(Ls, [V|Rs], Xs),
    length(Ls, N),
    ( length([V|Rs], N) ; length(Rs, N) ; fail),
    list_tree(Ls, L),
    list_tree(Rs, R), !.

