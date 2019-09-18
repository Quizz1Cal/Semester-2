# Hidden Flow in Data Types

**Static Type Checking**: A specification for types. These types of systems COMPARES the implementation to the specification and ensures the program is consistent with type.

## Type System Designs

To read: Stephen Dolan (2016) PhD thesis.

### (Global) Type Inference: Haskell, let-polymorphism

IDEA:
- Associate type variables with each node
- Derive relationships (constraints) b/t nodes
- Solve constraints by unification

### (Local) Bidirectional Type Checking: Java

## Subtyping
- Nested types: more specific treated as less specific
    - E.g. `Int <= Num`
    - E.g. `Int <= Object`
    - E.g. `{:a Int, :b Bool} <= {:a Int}`
- Difficult unification, so hard to apply to global type inference

E.g. `(map (fn [x] x) [1,2,3])` can't always figure out the type of `[x]`.

Data Flow: how the inputs/outputs within a function (type) flow.
- Benefit to Global TI: If you want to subtype, COMPARE THE DATAFLOWS (if you can overlay, and trim, it's a subtype)
- Applying to bidirectional TI: 'Pause' the type checking of internal unknowns, use data flows to eventually figure it out

# Reflex Outside the Browser

## Jack Kelly
- QLD Functional Programming Lab
- Data61
- (Good link) [http://jackkelly.name/talks]

**Functional Reactive Programming**: Talks about time-varying vales and instantaneous phenomena
- Reflex implements this
- E.g. `Behaviour a`, `Event a`, `Dynamic a` model time-variance behaviour, instantaneous occurrences/behaviours of a, and dynamic a which is an extended version which behaves like a sum of activations (H-kind)
    - Note that Behaviour, Dynamic are Monads
    - Note that Event is ONLY functor, but it is Filterable and Semialign (I can operate on this/that/these-es)

**Challenges of Reflex**:
- Spectacular type sigs
- Pigeonholed as front-end
- Reflex-platform (nix)

TODO: Look at Generics

# An Intuition For Propagators

Motivations:
- bidirectional/cyclical flow of information. This was hard to express.
- Need for 'guessing' in these networks was annoying

**Definition**: From MIT. A MOC for highly parallel machines. (which is more or less distributed, parallel systems ATM)

## Propagator Networks
- Collection of cells (a place where info goes) in a network

**Propagator**: Looks at cells, takes info from some cell, outputs to another. **INDEPENDENT STATE MACHINE**, highly functorial.
- e.g. `lift toUpper input output` (in a do)
- e.g. `adder l r o = do $ lift2 (+) l r o`

If you model equations as `z <- x+y`, `x <- z - y` etc. then you can have a bunch of propagators that propagate incomplete data! It works in bidirectional, cyclic networks.
- Oscillation does work; furthermore, when in parallel, does so nondeterministically

### To Deal With Nondeterminism

IDEA 1: AD-HOC - `Perhaps a = Unknown | Known a | Contradiction`, yield a contradiction if you `tryWrite` some non-equal value

### Accumulation = Mathematical Solution
"A cell should not a value, but rather, everything I know about a value."
- Writing to a cell == adding new information
- "Parallelism"
- E.g. model sudoku with cells containing the nondeterministic potential values. Apply propagators of elimination to model constraints.
    - Note that in information hierarchies, some levels are M.E. but b/t levels can be incomparable
    - Every two pairs of 'possibilities' in an info hierarchy has a unique upper bound + partial orded = *bounded order semilattice*
    - Cells contain a semilattice; propagators join info in

In general, use a semilattice to model 'all transitional pathways of information' (i.e. you can model the Perhaps like this).
- Applies to Sets, Intervals, Bidirectional Equations

### Extras
- New research = "Doesn't have to be a bounded order semilattice"
- Applies to laziness, search, unification, integer LP, SAT solving
- TODO: Investigate who is curious about propagator networks
- Art of the propagator
- Alexey radul's PhD thesis
- ekmett/guanxi -> modern, fancy, experimental implementation.
- Seems to overlap with FRP: 
- Relates to finite-domain constraint solving
- Optimisations include Scheduling the firing of neighbours

Gerald Sussman


## George Wilson
- Also CSIRO Data61

# Crashkell

Will always return first result since dog is effectively _
```haskell
result = case "x" of
    dog -> ...
```

You are restricted to simple patterns in case clauses. (so no ..)
Try an if-statement here instead.

Tony Morris, George Wilson
Proof by Paramtricity
Catamorphism
Djinn
