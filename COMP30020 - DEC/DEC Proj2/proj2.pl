/* proj2.pl

:purpose: 
Implements the constraint programming logic for verifying and searching for 
the solution to a math puzzle, a finite domain problem.

:author: Callum Holmes, 899251

:Math puzzle & constraints:
A math puzzle is an (M+1) x (M+1) matrix/array, where M is some 
integer greater than 1, with these constraints:
- Each row/col has unique integers valued from 1 to 9
- The principal diagonal has identical values
- Each leading value in each row and column (the 'header' values) are EITHER:
    - The sum of integers of the row/col it leads OR
    - The product of integers of the row/col it leads
- A 'line' as referred to in this code refers to a lsit with a header value and
  the corresponding list of nonheader values

It is these constraints that are used to search for valid solutions.

:module requirements:
- Access to the CLP(FD) library for constraint logic
  programming for Finite Domains.

:assumptions:
- That only one valid solution can exist (as per the specifications) where 
  some values are variables/non-determined, so it is enforced to only
  return one solution by use of a cut operator (!).
- That the 'header values' for rows and columns are always known
- The math puzzle is dimensioned and constrained as above
*/

% Library imports
:- use_module(library(clpfd)).

/* puzzle_solution(+Puzzle)
  :purpose: 
  - For full puzzle, tests if a valid solution
  - For a partially completed puzzle, searches for a solution.
  :input: 
  - Proper list of rows of a math puzzle table (represented as lists)
  :assumptions:
  - Input is proper list of proper lists
  - Only one valid solution (if any) 
  - All 'header' values are known
*/
puzzle_solution([H1|Rows]) :-
    % Enforce diagonal constraint 
    % first diagonal is in headers (redundant), so not included in Ds
    diag([H1|Rows], [_|Ds]), all_equal(Ds),

    % Enforce all rows and cols satisfy value constraints
    maplist(valid_line, Rows),
    transpose([H1|Rows], [_|Cols]),
    maplist(valid_line, Cols), 

    % Label unknowns where multiple values still possible after constraint
    append(Rows, Vals),
    include(var, Vals, Vars),
    label(Vars).

/* valid_line(+Line)
  :purpose:
  - Enforces that a list (row/col) has the math puzzle properties that
    1. All nonheader values are between 1 to 9 (inclusive)
    2. All nonheader values are unique
    3. That the header is either the sum or product of the nonheader values
  :input:
  - A list of values, with header as its first element
  :assumptions:
  - The header value is ground
 */
valid_line([H|Vals]) :-
    maplist(in_domain, Vals),
    all_distinct(Vals),
    ( is_product(Vals, H)
    ; is_sum(Vals, H)
    ).

/* all_equal(+Vals)
  :purpose:
  - Enforces that a list has identical values
 */
all_equal([_]).
all_equal([V1, V2|Vs]) :-
    V1 #= V2,
    all_equal([V2|Vs]).

/* is_product(+Vals, ?Product)
   :purpose:
   - Checks if the product of a list's values is header value H
 */
is_product([V], H) :-
    H #= V.
is_product([V|Vs], H) :-
    H #= H1 * V,
    is_product(Vs, H1).

/* is_sum(+Vals, ?Total)
   :purpose:
   - Checks if a list sums to header value H
 */
is_sum([V], H) :-
    H #= V.
is_sum([V|Vs], H) :-
    H #= H1 + V,
    is_sum(Vs, H1).

/* in_domain(?V)
   :purpose:
   - Enforces the domain of a nonheader value
 */
in_domain(V) :-
    0 #< V, V #< 10.

/* tail(?List, ?Tail)
  :purpose:
  - Extract tails from a list.
  :assumptions:
  - List is non-empty
 */
tail([_|Tail], Tail).

/* diag(+Matrix, ?DiagonalList)
  :purpose:
  - Extracts diagonals from a matrix by iterated trimming
  :assumptions:
  - Matrix is a list of lists of equal length
 */
diag([], []).
diag([[D|_]|Rows], [D|Ds]) :-
    maplist(tail, Rows, Rows0),
    diag(Rows0, Ds).

