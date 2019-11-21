# Prolog Coding Summary

### Author : Callum H

Prolog = "Programming in Logic".

# Key Ideas:
- Write code declaratively: write it as if you were checking/asserting the result's correctness, rather than computing it.
- In general, you don't need to specify when predicates fail; the tradeoff being, you must be exhaustive for success.
- Each Prolog clause should be a true statement about the predicate it's defining independent of the other clauses.

# Features of Prolog
- Dynamic typing: do NOT declare types
- Single-assignment: no doubling up on definitions
- Side-effects are present (e.g. IO)
- Driven by logical inference
- Use of backtracking for choicepoints
- Last-call optimisation / Tail-recursive optimisation

# Basic Syntax, Datalog

## Scope

Variables are limited to the scope of the rule (clause) they are in.

## Unification

The process of verifying/substituting values (see below). This IS pattern matching as seen in Haskell, and is attempted linearly: if the first mention of `parent` (or any other predicate/rule) fails to unify, try the next, until total failure.
- E.g `[a,b,c]` unifies with `[Head|Tail]` resulting in `Head = a` and `Tail = [b,c]`

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

**Cut**: The cut operator `!` is a break for Prolog.

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

**Terms** are data structures, of the atomic, compound or variable variety. (Datalog doesn't have compound).
- Atomic terms: Floats, Integers and atoms
- Integers can be written with `_`; these are ignored, are only for readability.

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

Note that the `-` constructor (e.g. `A-B`) is analogous to the use of `,` in Haskell for tuples.

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
- `append/2` unstacks lists

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
- can also use **same_length(X,Y)** to check if two lists have same length.

To check list membership (is builtin): 
- `member(Elt, List) :- append(_,[Elt|_], List)`
- ALT: `member2(Elt, [Elt|_]). ... member2(Elt, [_|Rest]) :- member2(Elt, Rest)`

To check range: use `between/3(+Low, +High, ?Value)`

**Sorting:**
- `sort(List0, List)` where List is sorted, UNIQUE, List.
- `msort(List0, List)` where List is sorted, same elements.
- `keysort(List0, List)` where List0 are *key-value pairs* (`X-Y`) and List sorted by Keys, NOT by values. STABLE SORT

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

Simulate while loops with the always-true repeat. The reason the whole thing is repeated lies in backtracking due to failure in termination condition.

```Prolog
predicate(...) :- repeat,
               ( "Termination Condition" ->
                    "Termination actions"
               ; "Cycle actions", 
                 predicate(new ...)
               ).
```

**If -> then ; else** is achieved as `(p -> q ; r)` (a goal). No choicepoint is created; it's either q or r. Great for deterministic code that needs to avoid choicepoints, and many solutions WON'T satisfy p.
- CON: If `p` has many solutions, only the first is committed to. So AVOID where multiple tests would be desired.

```Prolog
ints_between(N0, N, List) :-
        (   N0 < N
        ->  List = [N0|List1],
            N1 is N0 + 1,
            ints_between(N1, N, List1)
        ;   N0 = N,
            List = [N]
        ).
```

## Higher-Order Predicates

**call/123** is used to execute its argument as a Prolog goal (with remaining inputs as additional args to it).  Thus, you can assign a goal to a Term and evaluate it later. i.e. `Goal = append(X,Y,[a,b]), call(Goal).` equates to `append(X,Y,[a,b]).`
- Immediate benefit: you can actually see the call as well as the outputs associated, for each combination.
- This means you can make lists of goals like `[write('pear+')] and then use maplist to run them later.

Currying exploits (which is possible in Prolog):
- `Curried = append(X,Y), call(Curried,[a,b]).`

**maplist(+Pred, +List)** (Pred curriable) holds if Pred succeeds for all args in List.
- *maplist(+Pred, ?List1, ?List2)* holds if Pred(L1,L2) holds element-wise (i.e. its not a cartesian mapping but a linear zip).
- e.g. `maplist(enrolled, [a,c,d],Subjects)` returns any length-3 list such that a studies the first, c the second, etc. This is **map** because the final list (in a column-like nondeterminism) has the outputs of f applied to the respective inputs.
- e.g. `maplist(plus(1),L,[3,4,5]).`

```Prolog
% maplist idea for implementation
% Note how 'call(P,X,Y)` is more or less `P(X) is Y`, giving a zip illusion.
maplist(_,[].[]).
maplist(P,[X|Xs],[Y|Ys]) :-
    call(P,X,Y),
    map(P,Xs,Ys).
```

**op(+Precedence, +Type, :Name)** defines a new operator of type with precedence. You can provide a list of names that all alias this same operator. 

### Solution Predicates

**setof(Template, Goal, List)** outputs List, all variables in Template that satisfy Goal, in canonical order using `@=</2`. 
- If none satisfy, it fails (NOT an empty list).
- It can infer variables not in Template through backtracking, e.g. `setof(S, enrolled(Student, S), Subjects).`

**bagof(Template, Goal, List)** is same as setof but doesn't sort or remove duplicates. 

**findall** implicitly existential-quantifies anything not in Template and returns empty lists if no solutions found. 

*Existential quantifying* can be done with `^` in Goals.
- E.g. `setof(D-T, Subj^class(Subj,D,T), Lectures).` yields all date-times for any subject lecture. 
- Multiple quantifying possible by `setof(D, [Subj, T]^...)`
- Compound goals require brackets - `Subj^(enrolled(...), class(...))`

## Term Comparison

Hierarchy: Variables < Numbers < atoms < compound terms.
- Numbers compared numerically, atoms alphabetically
- Compound terms are compared by arity, THEN by functor, THEN first arg, etc. 
- Variables are compared on 'when they were created'. NOTE THAT BINDING SCREWS THIS, I.E. `X @< a, X = b` yields `X = b.`
- Operators: `@<` `@=<` `@>=` and `@>` (just a `@` in front of numerical comparators)

Use of double goal negation checks for possible unification w/o actually unifying:
- `\+ \+ X = Y, X=a, Y=b.` checks if X could= Y (true), thus the unifying `X=a, Y=b` occurs.

Use of `==` and `\==` checks if terms, INCLUDING THE VARIABLES, are identical WITHOUT binding AT time of the call/execution (so binding post-use is fine)
- E.g. `X \== Y, X=Y` says that X was not Y, but is now.

## Type Testing

- `integer(Term), float(Term), number(Term), atom(Term), compound(Term)` all test if Term is bound to their thing AT execution.
    - So `integer(X), X = 42` fails
- `atomic(Term)` roughly means `atom(Term) ; number(Term)`
- `var(Term), nonvar(Term)` and `ground(Term)` test for unbound, non-unbound, and containing zero unbounds respectively.

This allows for mode-sensitive code such as:

```Prolog
take(N, List, Front) :-
    (   nonvar(N)
    ->  length(Front, N),
        append(Front, _, List)
    ;   append(Front, _, List),
        length(Front, N)
    ).
```

## IO

This is **NOT** pure, nor logical - conjunction is non-commutative.
- Hence, should isolate I/O to a small part of the code (as we do in Haskell)

Use `write(Term), writeq(Term) and read(Term)` for writing WITHOUT quoting, writing WITH quoting, and reading from current input stream.

Use `get0(Char)` and `put(Char)` for char-by-char read/write. `nl` starts a new line on the current output stream.
- `get0/1` returns -1 at EOF and 10 at end of line.

Use `see(File)` (open to read), `seen` to close it, `tell(File)` to open for write and `told` to close it. Without this they write to terminal.

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

**Comments**: `%` for single line

Writing `some_predicate/#` is to denote a predicate that takes `#` no. of arguments.

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

## Semantics

The `meaning' (called semantics) of a logic program is what makes it true (set of logical consequnces as ground atomic formulas). For Prolog, the true meaning of a program is just a listing of all the clauses.
- To discover them, reason from the program to discover what ground queries succeed
- Call the *immediate consequence operator* T_P: it takes a set of ground clauses C and products set of clauses *implied* by C, together with program P.
    - E.g. P = {rules} and C = {clauses/atoms} then T_P(C) = {stuff implied that ISNT in C}... I think.
    - The semantics of a program is T_P(T_P(...(null)))

Where rules are used, to extract the semantics, find all possible input combinations and write out a corresponding clause for each.

## Recursion Considerations

If only want/expect one solution, make predicates deterministic (no choicepoints). E.g. no base and recursive cases, just one case. Otherwise, you'll disable LCO.
- E.g. for factorial: you have `fact(0,1).` and `fact(N,F) :-` then Prolog will check 0 against both ... and give you issues on the second (unless you catch it)
- Avoid with if-then-else constructs OR (better) indexing

## Logic and Resolution

Two interpretations of predicates:
- Function mapping all combinations of terms to a truth value
- Set of tuples of terms (those within implicitly true)

Interpretations of clauses:
- See MOC

Procedural Interpretation:
- Logical: as you would in MOC
- Procedural: To show that (consequent), it is sufficient to show that (antecedent) (SLD resolution)

### SLD Resolution

IDEA: Given program and some target goal, infer what is sufficient to show. This is achieved through rule and unification resolving in an SLD Tree.

At each step, can either:
1. Continually resolve rules into resolvents (the antecedents) by unifying on terms/variables (direct descendant)
2. Choose to pursue a (sub)goal - leads to another path in an SLD Tree (branching)

Prolog always selects the first goal to resolve and selects the first matching clause to pursue first (as we already knew from backtracking and order of execution); this is aided with choicepoints.

**Indexing**: Allows Prolog to jump (immediately) to matching clauses, based on mode. 
- Changes PERFORMANCE, not BEHAVIOUR (it gets smarter about what to skip, but it is still swept in same order)
- Not all systems index on the same # of arguments.
- Can mitigate choicepoints algother

## Tail Recursion

**Definition**: Any predicate/function whose recursive call on ANY execution is the LAST code executed before returning.

**Tail Recursion Optimisation (TRO)**: Used to optimise recursion, effectigely converts recursive predicates behave like loops. 
- Note that many programming languages store local vars and where to return in a stack frame: fresh stack frames are created at each call.
- TRO is a special case of **Last Call Optimisation**: It skips stack frames for functions that would only call another function and then promptly return

With TRO, tail recursive predicates execute in *constant stack space* (like a loop). Choicepoints ruin this (it freezes it and earlier frames).

IDEA 1: Use an accumulating parameter to aid in defining tail-recursive functions.

```Prolog
% Tail recursive factorial
fact(N,F) :- fact1(N,1,F).
fact1(N,A,F) :-
    ( N > 0 ->
        A1 is A * N,
        N1 is N - 1,
        fact1(N1, A1, F)
    ; N =:= 0 ->
        F = A
    ).
```

IDEA 2: Accumulation + Unfolding - start with `recurse_call(args), final_computation`, then actually write out the recurse_call, push the computation into any decision paths, and simplify. With some shifting, you should be able to sub the 'start' but one iteration ahead (folding) to replace computations and achieve recursion.

IDEA 3: Finding the new predicate - look for a function that combines the 'desired' work with the additional computations preventing it from being tail-recursive. E.g. `rev,append` becomes `revappend(BCD,A,DCBA)`
- ALT: The 'extra argument' is what should come AFTER the list operation (helps with appending); if not known, pass an unknown. 

## Constraint Logic Programming

Constraint programs are more declarative than logic programs as they allow more general constraints than mere equality - search follows an algorithm.

**Problem Specification:**
- Set of variables with unknown domains for each
- Set of constraints, with each involving one or more variables
- Optional objective function
- The constraint programming system attempts to find a solution (assignments to variables from domains) satisfying constraints AND pick most optimal solution based from the objective function.

**Types of CP**:
- Herbrand CP: Variables are terms, constraints are unifications (PROLOG)
- Finite domain CP (FDCP): Variable domains are finite. Use  `library(clpfd)`
- Boolean satisfiability (SAT): Variable are booleans, constraints assert truths 
- Linear inequality CP: Variables are reals (sometimes ints) and constaints of form ax+by \leq c (half-planes in R^2) with a,b,c constant. use `library(clpr)`

### Type Inference with CLP

Haskell uses Hindley-Milner type system, which can be approached with Herbrand CP. 
- Types are Herbrand terms; rules are  used to create equality constraints for each subexpression in a function
- TO SIMPLIFY (Core Haskell): Assume program is normalised, where all nested expressions have been replaced by let-bound variables, and deconstructions (e.g. pattern matches, overloadings) by case constructs

E.g. for `map f l = ...` it must be true that `t_map = t_1 -> t_2 -> t_r` (an equality constraint on those type variables)
- Make these assertions for EACH let-assignment, clause, internal variable, case condition and case result. 

```Prolog
% Haskell code
% map f l = case l of 
%     []     -> 
%         []
%     (e:es) -> 
%         let fes = map f es
%                   fe = f e
%             in fe:fes

% Prolog
?- op(200,xfy,'=>'). % arrow op; right-associative
?- Map = F => L => Result,  % Assign some variables to types of arg/result
|    L = [_], % case 1 on L
|    Result = [_],  % result 1
|    L = [E], Es = [E],  % case 2 on L forces Es
|    Map = F => Es => FEs,   % fes type due to map f es
|    F = E => FE,   % fe and e relation due to $
|    Result = [FE],  % result 2 
|    FEs = [FE].   % fes and fe relation due to :
Map = (E=>FE)=>[E]=>[FE],
F = E=>FE,
L = Es, Es = [E],
Result = FEs, FEs = [FE].
```

### Search

Prolog usually employs 'generate and test' to search for variable bindings.
- Nondeterministic goals generate potential solns; goals TEST these.

CLP uses 'constrain and generate': 
- Constriants can be more sophisticated than a simple Herbrand term bind
- Achieved in Prolog with *attributed values* (allowing constraint domains to control unification)

Patterns observed in the discussion of Timetabling:
- Too many constraints can prevent any solution being found; optional constraints therefore should be applied AFTER essential constraints have their fun

### Propagation

Solving FD constraints typically achieved by two steps:
- Propagation: Try to reduce domains for each constraint by checking the rules eliminate any potential values left; on each elimination, repeat this check.
    - Ends if variables all have domain size 1, a variable has no domain, or constraints are exhausted
- Labelling: Picks an unfixed variable and partitions its domain into parts, invoke the solving algorithm for each part, restricting this variable JUST to that domain. Effectively, this is a search tree.

# Questions/Random
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
