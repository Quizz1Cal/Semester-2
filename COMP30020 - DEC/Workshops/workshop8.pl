% Workshop 8

% Tail-recursive sumlist
sumlist(List, Sum) :-
    sumlist(List, 0, Sum).

sumlist([], Sum, Sum).
sumlist([L|Ls], Sum0, Sum) :-
    Sum1 is L + Sum0,
    sumlist(Ls, Sum1, Sum).

% 
tree(empty).
tree(node(Left,_,Right)) :-
    tree(Left),
    tree(Right). 

tree_list(empty, []).
tree_list(tree(L,V,R), List) :-
    tree_list(L, LList),
    tree_list(R, RList),
    append(LList, [V|RList], List).


% balanced tree
% list_tree()
list_tree(List, Tree)