% workshop 9

% Cheating
same_elements(L1, L2) :-
    sort(L1, Sorted),
    sort(L2, Sorted).

% By hand: just code an 'allin' func

% Q3
times(W,X,Y,Z) :-
    ModW is abs(W) - 1,
    between(0,Y,ModW),
    Z is W*X + Y.

% Q4
% containers(Moves).

% Close, but should have used arguments to represent actual capacity.
containers(Moves) :- 
    run(Moves, 0-0, []).

run([fill(To)|Ms], L-R, Prevs) :-
    \+ member(L-R, Prevs),
    Prevs0 = [L-R|Prevs],
    ( To is 3 ->
      run(Ms, 3-R, Prevs0)
    ; run(Ms, L-5, Prevs0)).

run([empty(From)|Ms], L-R, Prevs) :-
    \+ member(L-R, Prevs),
    Prevs0 = [L-R|Prevs],
    ( From is 3 ->
      run(Ms, 0-R, Prevs0)
    ; run(Ms, L-0, Prevs0)).

run([pour(3,5)|Ms], L-R, Prevs) :-
    \+ member(L-R, Prevs),
    Prevs0 = [L-R|Prevs],
    Total is R + L,
    ( 5 =< Total ->
      Rem is Total - 5,     
      run(Ms, Rem-5, Prevs0)
    ; run(Ms, 0-Total, Prevs0)).

run([pour(5,3)|Ms], L-R, Prevs) :-
    \+ member(L-R, Prevs),
    Prevs0 = [L-R|Prevs],
    Total is R + L,
    ( 3 =< Total ->
      Rem is Total - 3,
      run(Ms, 3-Rem, Prevs0)
    ; run(Ms, Total-0, Prevs0)).
      
run(_,_-4, _).      
