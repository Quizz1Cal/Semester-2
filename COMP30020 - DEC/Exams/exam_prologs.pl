% Prolog
% intset_insert(+N, +Set0, ?Set)
intset_insert(N, empty, tree(empty, N, empty)).
intset_insert(N, tree(L0, M, R0), tree(L1, M, R1)) :-
    (  N < M 
    -> R0=R1, intset_insert(N, L0, L1)
    ;  ( N > M
       -> L0 = L1, intset_insert(N, R0, R1)
       ; M = N, L0=L1, R0=R1
       )
    ).