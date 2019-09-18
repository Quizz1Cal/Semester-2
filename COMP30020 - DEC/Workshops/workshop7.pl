% Workshop 6

% Takeaways:
% Feel free to do func(X,X,etc.) - it will match same.
% Very clever append solution.
% The 'control flow' of the program should be in the rule HEADS, not body.
% Q: is my solution 'okay' by Prolog or is it too imperative

% list_of(?Elt, +List).
list_of(_, []).
list_of(Elt, [Elt|Ls]) :-
    list_of(Elt, Ls).

% all_same()
all_same(List) :-
    list_of(_, List).

% adjacent.
% Very clever.
adjacent(E1,E2,List) :-
    append(_, [E1,E2|_], List).

% adjacent
% Either they perfectly match... or they don't, so throw the first out 
% And try again.
adjacent2(E1,E2,[E1,E2|_]).
adjacent2(E1,E2,[_|Tail]) :-
    adjacent2(E1, E2, Tail).

% before
before(E1,E2,[E1|List]) :-
    member(E2, List).
before(E1,E2,[_|List]) :-
    before(E1,E2,List).

% tree(L,N,R)
intset_member(+N, +tree(L,V,R)) :-
    N = V ;
    N < V, intset_member(N, L);
    N > V, intset_member(N, R).

% intset_insert(+N,+Set0,?Set).
intset_insert(N, empty, tree(empty, N, empty)).
intset_insert(N, tree(L,V,R), Tree) :-
% intset_insert(N, tree(L,N,R), tree(L,N,R)) would be alt. to the first bit here.
    N = V, 
        Tree = tree(L,V,R) ;
    N < V, 
        intset_insert(N, L, NL), 
        Tree = tree(NL, V, R);
    N > V, 
        intset_insert(N, R, NR), 
        Tree = tree(L, V, NR).