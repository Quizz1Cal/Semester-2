# Notes for Declarative Programming 2019 Sem 2

### Author: Callum H

## Functional Programming (Haskell)

**Imperative languages**: Based on commands, instructions, statements; have effects, side-effects. High degree of mutability.

**Functional languages**: Based on expression, are evaluated, no effect. Typically all data structures are immutable (no modification)

Features of functional programming:
- Relies on/basis is **Equational reasoning**: Two expressions with equal values can replace each other; there is no uncertanty about an expressions' meaning if an equal sign
- **Referential Transparency**: An expression can be replaced with its value. Requires that *has no side effects* and *is pure (consistent result return for same input)*
- Variables are *single assignment*, and there are no assignment statements. 

**Side-effect**: Code that produces a value and simultaneously modifies some state/observable intxn with calling function/outside world.

**Church-Rosser Theorem**: Regardless of the order of written subterms/reduction, the final result is the same.
- For an imperative language since the order of evaluation (due to side effects) almost always changes result

## Declarative Prog. Techniques

**Nth Order Function**: Functions whose arguments/results are n-1th or lower order.
> First: Data
> Second: Functions of data
> Third: Functions of data & functions
etc.

PLEASE REMEMBER ERROR m HAS ADJUSTABLE TYPE DEP. ON CONTEXT

## Extra Notes

Monad Notes:
- **Monads** represent a computation - programmer determines the composition of operations. 
- `>>` ignores the value of the first operand, only returns value of second
- Functions that return `IO t` are best thought as:
    1. Return a value of type `t` (hidden)
    2. A description of an I/O operation (side effect)
- Compiler/runtime strive to execute the I/O's ASAP given that:
    1. Description is made in a context s.t. the dscription ends up in the list of operation descr. returned by main
    2. All previous operations in this list have been executed
    - The motivation: don't want to execute actions not called for, nor out of order
- Execute many actions iteratively by storage in list, followed by another function with a do that executes them
- Advantage of Haskell's type system:
    - Unit tests for non-IO functions are highly replicable/easily checked with variable substitution and comparison
    - Non-IO code rearrangeable and parallelisable
- DEBUGGING NON-IO: `unsafePerformIO :: IO t -> t` (GHC.IOBase) tests a IO and returns its value. This is unsafe because you can't deterministically tell how it fits into the main sequence

State Monad Notes:
- `Control.Monad.State.Lazy`
- Functions of form `val -> s -> (val,s)` are perfect for composition
- A state is a FUNCTION. Yes it's an 'object' but that is hard to interpret.
> If you have a `(State s a)` you don't have a state, you have a state generator.
> If you have an `a` you have an underlying data field
> If you have an `s` you have an underlying state.
> If you have an `a -> (State s a)` or `a -> s -> (s,a)` you have a state-based computation (`>>=`) 
- TODO: Lookup `runState` and `State` (builtin but I can't see where)