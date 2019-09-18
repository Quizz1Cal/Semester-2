# Prolog Coding Summary

### Author : Callum H

# Key Ideas:
- Write code declaratively: write it as if you were checking/asserting the result's correctness, rather than computing it.
- In general, you don't need to specify when predicates fail; the tradeoff being, you must be exhaustive for success.

# Features of Prolog
- Dynamic typing: do NOT declare types
- Single-assignment: no doubling up on definitions
- Side-effects are present (e.g. IO)
- Driven by logical inference

# Basic Syntax, Datalog

Prolog = "Programming in Logic".

**Comments**: `%` for single line

**Spacing/indentation**: ...

Writing `some_predicate/#` is to denote a predicate that takes `#` no. of arguments.

## Scope

Variables are limited to the scope of the rule (clause) they are in.

## Unification

The process of verifying/substituting values (see below). This IS pattern matching as seen in Haskell, and is attempted linearly: if the first mention of `parent` (or any other predicate/rule) fails to unify, try the next, until total failure.
- E.g `[a,b,c]` unifies with `[Head|Tail]` resulting in `Head = a` and `Tail = [b,c]`

### Cut

The cut operator `!` is a break for Prolog.

## Operators

Comparative:
- **Assignment and equality** both use `=`, and is single-assignment like Haskell.
- **Disequality**: `\=`. Note the equivalence to `\+ X = Y`
- Also have `<,>,>=,=<` (WATCH THE WEIRD `=<`)
- `=:=`, `=\=` is (eq\diseq)uality specifically for comparing VALUES (numbers, for example)

Arithmetic:
- `*,+,-` are standard. Negation is standard.
- `/` can yield float, `//` for integer rounding down
- `mod` (same sign as arg 2)
- **IMPORTANT**: Use `is` to evaluate; `X is 6*7.` yields 42 but `X = 6*7.` does not. 
    - So `square(N,N2) :- N2 is N * N.`
    - Requires the second argument to be ground, so no backward inference for `square`

## Datalog Basics

**Relation**: A relationship. `parent(me, you).` A relationship's name is a **predicate** (e.g. `parent`). 
- A predicate can have **bound** (non-variable) arguments or **unbound** (variable) arguments
- Can by defined with any number of clauses of any type combination
- E.g. `queen(queen_elizabeth).`
- Assertions like the example don't have to declare `queen_elizabeth` prior to it.

A **clause** is a "logical object" that can, with others, form a predicate; it includes **rules** or **facts**.
- Executed first to last
- **Rules** are of the form `Fact :- Query`. The `:-` is 'if', i.e. logical consequence of. 
    - Recommend converting all worded questions into rules and facts
- **Fact**: A statement. **Ends with a fullstop.** (of course, complex structures have fullstop at the end of the entire fact)
- E.g. `parent(a,b).` or `this_is_valid.`

### Querying

**Query**: Looks like a fact, but is in fact a question/statement that Prolog will determine is `true.` or `false.`
- Executed first to last
- Queries with a variable will attempt to find all values for the variable that the statement is true.
- **Mode**: Explains the 'format' of a relation, based on what arguments are bound/unbound. 

**Compound Query**: Combines multiple `goals` (a.k.a. atoms; just predicates really), separated by a comma (**INTERSECTION/INNER JOIN**) or semi-colon (**DISJUNCTION/FULL OUTER JOIN**)
- Conjunctions are higher precedence than disjunctions; parentheses for grouping
- Negation: `\+` prefix operation and is tighter

```Prolog
% Example query for 'Prince Charles is the parent of who?`
?- parent(prince_charles, X), child(X, prince_charles).
% OUTPUT
X = prince_william ; % MUST TYPE ; MANUALLY
X = prince_harry.

?- parent(X, prince_william), X \= prince_charles.
```


## Closed World

Prolog will assume 'all true things can be derived from the program'. 
- When negating goals: a failed goal will succeeed (*negation as failure*) so be careful. 
- Furthermore, a failing goal can NOT bind variables -> thrown away when `\+ G` evaluates G.
- THEREFORE bind any variables used in a negated goal prior to executing the goal I.e. on the left)

```Prolog
% Is there a person that Queen Elizabeth is NOT a parent? (i.e. if someone is found, G succeeds so the goal fails.)
?- \+ parent(queen_elizabeth, X).
false.

% Is there someone not Queen Elizabeth?
?- X \= queen_elizabeth.
false.

% Who are the people not Queen Elizabeth?
% The double parent clause is because all the names are not necessarily both children & parents, but together
% those fields scope all known people.
?- (parent(X,_) ; parent(_,X)), X \= queen_elizabeth.
X = prince_phillip ; ...
```


## Recursion

Has no looping mechanism, but lacks higher-order operations - so recursion is quite frequent.

```Prolog
% Example recursion
% A person's ancestors are their parents and the ancestors of their parents.
% Read as 'Anc is ancestor of Desc IF Anc is parent of Desc'
ancestor(Anc, Desc) :- 
        parent(Anc, Desc).
ancestor(Anc, Desc) :-
        parent(Parent, Desc),
        ancestor(Anc, Parent).
```

## Prolog Files / Using SWI-Prolog

SWI-Prolog does not accept new rules and facts on the top-level, it only accepts queries.

Rules are typically added by writing them in a text file (for example rules.pl), and load it into SWI-Prolog using `?- [rules].`

# Beyond Datalog

## Terms (Data)

**Terms** are data structures, of the atomic, compound of variable variety. (Datalog doesn't have compound).
- Atomic terms: Floats, Integers and atoms

**Variable terms** are variables; they denote a single unknown term.
- Start with CAPITAL/_ followed by alphanum or _.
- Like in Haskell, single `_` can be re-used for separate variables, and using `_` at the front indicates the variable though named is redundant.
- Can be used in compound terms

**Atoms** beginw with a lowercase, followed by alphanum/_. (e.g. `queen_elizabeth`). 
- Quotes can be used as well (e.g. `'Queen Elizabeth', 'Hello\tWorld!\n'`) to allow whatever.

**Compound Terms** are functors (function symbol) followed by zero or more arguments (terms). Brackets are used to enclose arguments, with commas. 
- Functors are effectively data constructors
- Syntax is same as an atom, so be careful
- E.g. `node(leaf, 1, node(leaf, 2, leaf))` for a small tree.
- E.g. `f(A,A)` (A is variable) is a term with functor `f` and two identical arguments.

Terms (single or compound) are called **ground terms** when no variables are used and **nonground** otherwise.

## Lists

Use `[]` for empty, `[1,2,3]` for standard definition, but `[X | Xs]` for a list with head `X` and the rest `Xs`
- `x1:x2:xs` matches to `[X1, X2 | Xs]` in Prolog

**Proper List**: Either empty or a singleton (list).

```Prolog
% Custom function to check if a proper_list
proper_list([]).
proper_list([_|Tail]) :-
    proper_list(Tail).
```

**Appending**: A QUERY. Like so: `append(l0,l1,OutputVar)`

```Prolog
% Custom definition. NOTE THAT IT IS INCLUDED
append([], C, C).
append([A|B], C, [A|BC]) :-
        append(B, C, BC).

% Used in backward appending
append([1,2,3], Rest, [1,2,3,4,5]) % yields Rest = [4,5].
append(F,B,[a,b]). % yields F = [], B = [a,b]; F = [a], B = [b]; etc. 

% Really awesome pattern matching (unification)
Front = [_,_], append(Front, Back, [1,2,3,4,5]).
Front = [1, 2],
Back = [3, 4, 5].
```

**Length**: Do like `length(..., Len)`.
- Note that `length(List, 3)` spits out `List = [_G273, _G276, _G279]` (which indicates three variables, numerically ordered in time encountered)

To check list membership (is builtin): 
- `member(Elt, List) :- append(_,[Elt|_], List)`
- ALT: `member2(Elt, [Elt|_]). ... member2(Elt, [_|Rest]) :- member2(Elt, Rest)`

## Control 'Flow'

```Prolog
% Get a list (desc. order) of N to 1, use 
new_collect_to(0,[]).
new_collect_to(N,[N|T]) :- N>0, N1 is N-1, new_collect_to(N1,T). 
```

**Cuts** (`!`) prevent backtracking:
- variables won't take on other values
- predicates before the cut will not be rechecked
- Only what is left in the rule with the cut will be evaluated

A **green cut** is used only for optimised evaluation. **Red cuts** alter the logical structure/flow of a program, and should be avoided where possible/sensible.

Simulate control flow with If-then-else: `s :- p -> q ; r.` This is equivalent to 
```Prolog
s :- p, !, q.
s :- r.
```

Simulate while loops with the always-true repeat. The reason the whole thing is repeated lies in backtracking due to failure in termination condition.

```Prolog
predicate(...) :- repeat,
               ( "Stuff to be iterated" ), ...
               ( "Termination Condition" ),
               !.
```

## Examples

```Prolog
% Custom take function. Assert that Front is a taking N of List
take(N, List, Front) :-
    length(Front,N),
    append(Front, _, List).

% Custom (naive quadratic) reversal. NOTE reverse/2 is builtin to SWI, reverse to 'standard'.
rev1([],[]).
rev1([A|BC],CBA) :-
    rev1(BC,CB),  % Defines CB
    append(CB,[A},CBA).  % Asserts CB relation to A and CBA

% Filter a list for only values above 6. 
% Note the exploitation of linear backtracking.  
% In the cirumstance below, reading :- as 'if ... then' or 'yields' may help.
sift([],[]).
sift([X|T],[X|Result]):- X > 6,    /* is X greater than 6 */
    sift(T,Result).                /* if so then go find the rest */

sift([ThrowAway|Tail],Result):-    /* disgard the head (b/c the above test failed) */
   sift(Tail,Result).              /* and look in the tail */
```

Use write/1 to output IN THE MIDDLE OF A TERM to output. nl/0 prints a newline.

```Prolog
% print_to(N) - prints out all the numbers down from N to 0
print_to(0) :- write(0).
print_to(N) :- N>0, write(N), nl, N1 is N-1, print_to(N1).
```

## Comment/Documentation

(From Grok): ``The convention is to document each Prolog predicate in a comment before the predicate definition beginning with one or more lines giving the form of a call to the predicate with a descriptive variable name for each argument preceded by a character indicating the mode for that argument. The characters used in the simplest convention are:

+ indicates an input argument; this means the argument is normally expected to be bound when the predicate is called (i.e. ALREADY being used)
- indicates an output argument; this means the argument is normally unbound when the predicate is called, and if it is bound, it will be unified with the output (i.e. NOT ALREADY used)
? indicates that the predicate may be input or output or both."

```Prolog
% append(+List1, +List2, -List3)
% append(-List1, -List2, +List3)
% List3 is a list of all the elements of List1 in order followed by
% all the elements of List2 in order.
```

## Semantics

The `meaning' (called semantics) of a logic program is what makes it true. For Prolog, the true meaning of a program is just a listing of all the clauses.

Where rules are used, to extract the semantics, find all possible input combinations and write out a corresponding clause for each.

## Compilation and Debugging

Use `?- [filename]` to compile (`?-` is the prompt)

Use `;` to read further answer(s) from queries and return key to finish.

### Debugging

Activate with `trace.`. Uses the Byrd box model: either `call` (entry), `exit` (successful completion), `redo` (backtrack) and `fail` (final failure).
- Turn off with `nodebug.`

# Outside of Prolog

**Substitution**: A mapping from **VARIABLES** (NOT atomic/compounds) to terms.
- Terms that are the result of a substitution to some other term are called *instances* of it. Denote the instance `tTHETA` yielded from applying substitution `THETA` to the term `t`
    - If you can find a `THETA` so that `u=tTHETA` then `u` is an instance of `t`
    - **GROUND TERMS HAVE (ONLY) ONE INSTANCE; NONGROUND TERMS HAVE INFINITE INSTANCES**
    - Don't have to substitute all variables - so nonground terms can be instances

**Unification**: Terms `t` and `u` are unified by substitution `THETA` iff `tTHETA = uTHETA`

# Questions/Random
- Integers are `50_000_000`????
- With the append thing, you can't next the append instaed of the BC thing?
- Apparently these unify and I don't like it: `[a,[]] and [A,B|Rest]` -> Yes `A = a, B = [] and Rest = []`
- WTF IS 
```Prolog
pu([],[]).
pu([A,B|Xs],[A-B|Ps]) :- pu(Xs,Ps).
```

Random:
- Singleton variable warnings most likely a typo
- `append([a,b,c,d],Y,Z).` yields `Z = [a,b,c,d|Y]`.
