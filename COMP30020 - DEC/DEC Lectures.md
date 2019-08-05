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

## Logic Programming (Prolog)

## Declarative Prog. Tools (debuggers)

## Interfacing to Imperative language code
 
## Constraint Programming
