# Haskell Notes

### Author: Callum H

## Features of Haskell
- Pure: No state, no side-effects, transparent building blocks
- Define functions with expressions
- All types are determined AT COMPILE TIME = strong type inference
- "If it compiles, it's correct"
- There isn't really a concept of a null value. 
- **Discriminated union types**
- **Strong, safe and static** type system:
> No loopholes (naughty casting illegal)
> Type errors don't happen e.g. wrong deferencing
> Types are checked/determined at compilation

### Expression Evaluation

This loop is the rough basis:
> Look for a function call in expression
> Search equations/patterns for this from top down for a match
> Set values to corr. parts of actual arguments
> Replace the function call in expression, with the RHS

## Core Syntax

### Common Operators 

Brackets are STRICTLY for grouping; spacing determines arguments for functions.

All punctuation-based operators are infix functions.

Commenting: `-- .....` and `{- multiline -}`

**infix**: takes arguments on left/right e.g. `+`. **Prefix** operators take arguments strictly on right.
> Convert infix to prefix with bracketing `(+) 1 2`. 
> Convert prefix to infix with \` e.g. `1 \`div\` 2`

Arithmetic operators: `*,^,/, div` (prefix), `mod` (prefix)

To negate numbers, must do as (-number) with brackets as functions have higher precedence.

Relational operators: `<,<=,>,>=,/=,==`

Logical operators: `&&, ||, not`

### Code Grouping (+ Offside Rule)

"The structure of the code as shown by indentation must match the code structure."

Offside rule = Implicit Grouping Mechanism

```Haskell
Code1 start here
Code2 A new construct adjacent to Code1
..Code3 Continues the construct above
Code4 EITHER Continues Code3 or is new & adj. to Code2...
```

Explicit Grouping:
```Haskell
let { x = ...
    ; y = ...
    }
in ...
```

### Functions

All functions in Haskell take one input and emit one output; anything higher order than this is a curried function.

Note that function type signatures are not mandatory but conventional for ease of reading/reference.

Cases for function input are possible with:
- Pattern-matching
- Guards
- Case

Function definitions are a list of exhaustive patterns/equations for input, of form `<name> <args..?> ... `
- **Exclusivity**: At most one pattern can apply for any possible call. A desirable quality
- Functions *can be defined inside other functions*
- Note that functions can be defined whilst backticked, however this does not make them infix functions (that's far beyond scope of course)

#### Pattern Matching

The below is analogous to a switch-case. 

```Haskell
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)
```

More complicated patterns:
- **As patterns**: Think of it as "Read-as": `f s@(x:xs) = x:s` recognises x AND s
- **Wildcard**: Use of `_` e.g. in `head (x:_) = x`
- Use of `(x,y)`, `x:y:z:[]` or `[x,y,z]`, `x:y:zs` and matching for data constructors is also possible

#### Currying

E.g. `Int -> (Int -> Int)`
- Curried Functions are of this form. Note the **functional argument**.
- In this context, the brackets are removable as the final output is `Int`
- Uncurried would be `(Int, Int) -> Int`
- Can convert with `curry` and `uncurry` builtins

#### Partial Applications

It can only occur in-order, sequentially. Given this, the idea is `f x` where f takes more arguments. This can then be fed the remaining arguments subsequently.

Functions that are partially applied are 'really' of type `f :: i1 -> ((i2, ...., in) -> rt)`.

Due to currying, partial application is *always* possible, even with mapping
- E.g. `map (*) xs` yields `[(<x0>*),<x1>*)...]` where each element is a partial function

**Section**: Partial application to a parenthesised infix operator with a left OR right operand
- E.g. `(map (*3) (x:xs)` or `map (5 'mod') (x:xs)`)
- For subtraction do `(subtract 4)` or `(+(-4))`

Application operator:
- $ for function application e.g. `fxn (1+1)` is equivalent to `fxn $ 1 + 1` 
- Also functionalises application or 'apply to arg' - e.g. allows  `map ($3) [succ, pred]`

#### Anonymous/Lambda Functions

As simple as `\x -> x+1` or `\s u -> s+u` Note the `\x` is the "lambda calculus" notation, the x is one input.
- This is a FUNCTION, not an expression.
- Naming can be done like any other function as `addOne = \x -> x+1`
- **WARNING**: Check you have no infix/prelude options, before coding a lambda

#### Composition

Use the `.` as such: `(f . g) x == f (g x)`
- Possible with partial application e.g. `sum . replicate 5 . max 6 $ 7`
- Last value must have an explicit function application

#### Pointfree Style

Idea: reduce the number of explicit arguments through function application and composition
- E.g. convert `fn x = tan (max 50 (cos x))` to `fn = tan . max 50 . cos`
- Not ideal for complex functions, but certainly ideal for 'stream'-like code

The opposite of this style would be binding-heavy, utilising `let`/`where` to break down problems.

### Expression Control Structures

#### If-then-else == Ternary operation

`if e1 then e2 else e3` is the standard expression (e1 is Bool). Effectively equivalent to
```Haskell
case e1 of True  = e2
           False = e3
```

The then/else must be on same line or or same indent as if: the e2 and e3 must be of same line or deeper indentation.

**WARNING**: The entire construct is an expression, NOT a statement: it will return either e2 or e3.

#### Do

#### Guards (the true conditional control)

```Haskell
fact :: Integer -> Integer
fact n
    | <0     = 0
    | n == 0    = 1
    | otherwise = n * fact (n-1)
```

When dealing with lists, can pattern match on `[]`, `[x]` or `(x:xs)`

#### Case

It is a switch-case equivalent. **WARNING**: This is an expression that evaluates to something; switch-case was mere control flow.
```Haskell
take m ys = case (m,ys) of
              (0,_)    -> []
              (_,[])   -> []
              (n,x:xs) -> x : take (n-1) xs
              _ -> some_default_expr
```

### Bindings

For both Let and Where:
- Patterns can be used e.g. `let (x,y,z) = (1,2,3) in x+y+z`, `where (f:_) = firstname` only grabs the `f`.
- Functions can be defined as such e.g. `where deadder a b = a+b-1`

#### Let

Binding is done as 
```Haskell
let pi  = 3.14
    tau = 6.28
in ...
``` 
where `...` is the context of its uses; it is a local variable.
- This is related to where as `... where pi = 3.14` achieves similar functionality
- However, while the above is an EXPRESSION, the use of `where` is not
- Can be used in list comprehensions alongside filters and generators, albeit what is bound cannot be used in the element pattern
- Cannot be used across guards (and so typically where is used for guards)
- The `in` can be omitted in GHCi if defining functions/constants

#### Where

An effective variable substitution. Aim to match indentation with guards (if any) and b/t each substituted term.
```Haskell
maximum :: [Int] -> Int
maximum [x] = x
maximum (x:xs)
    | x > maxxs = x
    | otherwise = maxxs
    where maxxs = maximum xs
```

An example where the function is defined post-use:
```Haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list." 
```

Where can only be used at the top level of a function.

### Types

#### Concrete Types

Concrete types include:
- Int, Integer (unbounded size)
- Bool, Char (note `String == [Char]`, strings use `""` and characters `''`)
- Double, Float (former is preferred)

Note that String is a *type synonym* for `[Char]` i.e. they are equivalent.

Tuples: `(a, b)`, `(a,b,c)` etc. Tuples of different length/composition are explicitly different types altogether.
> Use `fst` and `snd` to fetch 1st, 2nd elements of 2-tuples **only**

**Empty tuples are functions** e.g. `(,,)` is equivalent to `\x y z = (x,y,z)`

**Type Constructor**: A function that takes types as input, builds a new type. 
- `[]` and `(,)` are inbuilt
- Custom type constructors must be capitalized

**Parametric Inputs** are typically as simple as using `a` instead of an actual type. Haskell fills in blanks at runtime.

```Haskell
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

Some Prelude data types include:
- **Eithers** are informally defined as `data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)`. 
- **Maybes** are defined as `data Maybe t = Nothing | Just t deriving (Eq, Ord)`
> Note the additional functions `maybe, fromJust, isJust, isNothing, listToMaybe, maybeToList, mapMaybe` to use it
> **EXAMPLE USE**: Whenever a function could return 'null' (e.g. a search) use return type `Maybe a`

##### Lists

List types: lists `[a]`, nested lists `[[a]]`.

**Lists** must contain elements of equal type: no random mixtures like in Python. By convention they are stored as `xs`, `ys` etc.

Examples:
- Ranges: `[1..4]`, `['a'..'e']`, `[1,3..7]`, `[1..]`
> Don't use with Float/Double as imprecision leads to accumulated errors
- "Hello" (strings are equivalent to type `[Char]`)

Lists are comparable, and are compared lexicographically (1st index, then 2nd etc.). In ties, the longer list wins.

#### Casting

Though technically impossible there are some situations analogous to casting.

**Type annotation**: Where the code specifies the type of an expression output.
- E.g. `read "4" :: Int`
- Typically used when Haskell is unclear (e.g. reading)
- Can be used for type casting of unconstrained type
    - E.g. `2 - 1 :: Double` works (not fixed to `Int`)
    - E.g. `length xs :: Float` fails (`length` specifically outputs `Int`)

Can also use functions like `fromIntegral` to cast strict `Int`, `Integer` to work with other `Num`
- E.g. `length xs / 2` fails but `fromIntegral (length xs) / 2` works

#### Typeclasses

A **typeclass** is a collection of types with some shared property (think interface in Java).

Common examples:
 - `Num` types can do arithmetic: includes the `Integral` Int, Integer and `Floating` Float, Double (both are also valid typeclass)
 - `Ord` types comparison (including equality)
 - `Eq` can be equated `==`
 - `Show` can be converted to string rep. Note that **functions are NOT showable and thus cannot be printed in gchi**
 - `Enum` can be enumerated (Bool/Char/Nums, (), Ordering)
 > `succ` and `pred` fetch for Enumerable data
 - `Bounded` have bounds accessed by the "polymorphic constants" `minBound :: <type>`, `maxBound :: <type>`
> Works on tuples of Bounded items
 - `Foldable`
 - `Read` types can be extracted from a string representation and use `read` to do so

 These are used in type signatures as `<fxn> :: Num a => [a] -> a`, or `(Num a, Ord a)` ... 

 **WARNING**: There is no need to 'list out' the type parameters in function type declarations. This listing occurs only in data constructors.

 **WARNING**: When it compares to Ord: the comparison order is determined by the definition, and left-right order when the data constructor is equal OR not (**lexicographic ordering**).

##### Defining Classes & Instances

A class definition is structured as such:

```Haskell
class (<Optional parent class> a =>) <Name> a where
    (operator1) :: <type>    -- Just a declaration
    <defined operator2>      -- Default implementation possible
    ...
```

The specification of those functions automatically rolls the typeclass constraint into their type.

A *class instance* is declared like

```Haskell
instance (Eq m) => (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ == False
```

and multiple of these are possible.

For `Eq`, you need to completely explain when it is equal, or not equal, as the base definition is recursive.

In general, you need to pattern match against all data constructors and define behaviour - at which point your data type will be of that typeclass.

#### Type Synonym

**Type synonyms** are equivalent types, and can be substituted for each other. Facilitated with `type Name = expr`
- `type Pair = (Int, Int)`
- `String` is a synonym of `[Char]`

**Polymorphic type synonyms** are possible such as `type Pair x y = (x,y)`, which renders `Pair` a type constructor.
- Still a curried function so partials like `type IntMap = Map.Map Int` are allowed

#### Data Constructors

Facilitated with `data <TypeName> = <exprs..>`

**Data Constructor**: 

Examples:
- `data myBool = myTrue | myFalse`. The myTrue, myFalse are implicit arity-0 data constructors a.k.a *nullary* data constructors
- `data Point = Pt Float Float` has type name `Point`. 
> Its value/data can be `Pt Float Float` (this Pt is now a data constructor used for Points). 
- Recursive: `data List = ListNode Int List | ListEnd`. 
> This is to say a `List` can have value `(ListNode Int List)`or `(ListEnd)`, and that `List` could be `ListEnd` or another `(ListNode Int Sublist)`

Note: the ability to have `ListNode Int List | Empty`, i.e. a combination of DIFFERENT structures (due to disjunction (|) and conjunction ( )) means Haskell has **discriminated union types**. 

A function might be as below:
```Haskell
data Point = Pt Float Float

invert :: Point -> Point
invert (Pt x y) = Pt y x
```

Note that the type name and data constructor can have same name.

Inheritance of typeclasses by data types is facilitated as 
 ```Haskell
 data Name (param-types?) = expr
    deriving (Eq, Show)
 ```

 Alternately if you want to custom-define the mechanism:
 ```Haskell
 instance Show Name where show = (some fxn of type Name -> String)
```

**Polymorphic data constructors** are possible: e.g. `data List a = ListNode a (List a) | ListEnd`.
- Implicitly defines a new type constructor (e.g. `List Int`)

**WARNING: It is (now) impossible to add typeclass constraints to data constructors**

##### Record Syntax

The form is as below (of course one-line is possible):
```Haskell
data Person = Person { firstName :: String
                     , ...
                     } deriving (Show)
```

Use of record syntax automatically generates getter functions for each field so that `firstName person` is possible, and showing it will look more like a dictionary/map.

**Pattern matching on records** can be done as normal but also like `func (Person {firstName = f, ...}) n = ... `

#### Newtype

**newtype** is used when you want to construct a new type that 'wraps' other types, with one constructor, and one field.
- E.g. `newtype Zipper = ZipList {getZipList :: [a]}` is built of other types

Benefits:
- More efficient (Haskell can recycle old definitions rather than reconfigure a new type)
- Can facilitate easy access with getter functions via record syntax definitions
- Allows lazier Haskell than `data` in rare cases
- Can allow for greater versatility in instantiation (e.g. a tuple where functions act on the first member only; used with monoids where multiple a. b. functions work for a data type)

### Comprehensions

The looping method of Haskell. Written as `[expr | pattern <- list, filterfunc args]` or `[expr | generator, filter]`. Typically `[f x | x <- xs, g x]`

Lists are filtered FIRST, then the output generated.

More complex structures allow `[expr | gen, filter, second gen, second filter, etc.]`. 
- The variables of gens can be used subsequently (i.e. in generators after the fact)
- The sets of gen/filters work right to left.
- Deeper gen/filters run faster/first.

Example:
```Haskell
l = [ x++" "++y
    | x <- ["hello", "fly away", "come back"]
    , length x > 7
    , y <- ["peter", "matthew", "paul"]
    , length y < 7
    ]
```

Note the pattern matching in  `dot xs ys = sum [x*y | (x, y) <- zip xs ys]`

### Laziness

Haskell only computes ranges of a list/function when absolutely required.

### Modules

Haskell *programs* are a collection of **modules** which each contain functions, types and typeclasses.

**Importing** is essentially `import <name>` e.g. `import Data.List`
- For specific imports, do `import <name> (<f1>, ..) hiding (<g1>, ...)`
- To resolve clashes, qualify imports as `import qualified <name> (as <alias>)` so used functions are called as `<alias>.<fname>` (note the renaming can be done even without qualifying)

#### DIY Modules

File structure:

```Haskell
module <Filename>
(, <f to export>
, <datatype>(<constructor1>, ...)
, <noncreateable datatype> -- Can be used, just not as data New = ..
,
, ...
) where 

<code>
```

Directory structure:

```Haskell
<GroupFolder>
-- <SubOne.hs> ==> In this file, type 'module GroupFolder.SubOne'
-- <SubTwo.hs> ==> In this file, type 'module GroupFolder.SubTwo'
```

Other modules in the same directory can be imported using their (full) name.

## Modules Guide

### Prelude

Common: `max`, `even`, `min`, `length`, `null` (checks if list empty), `!!` (indexing), `++` (infix list concat.)
- Fun fact: `!!`, `length` run in linear time due to Haskell NOT having random access

Prelude slicing: 
- `head (x:xs)` gets x, `tail` gets xs
- `last (..x)` gets last elem, `init` gets prefix list
- `take n xs` gets first n, `drop n xs` gets remainder
- `takeWhile p xs` slices until the first instance `p` is False
> E.g. firstWord extractor

Operator extensions to lists: `and`, `or`, `product`, `sum`, `maximum` and `minimum` work on lists

Repeaters:
- `until <predicate> <func iteratively applied> base`
- `cycle xs` repeats a list
- `repeat x` repeats an item infinitely
- `replicate num x` repeats x num times

Higher order:
- `foldr (\x acc ..) acc xs` and `foldl (\acc x ..) acc xs` are reducers. 
- Use `foldr1` and `foldl1` if no base case is necessary
> Foldl progresses rightward and foldr vice-versa using an accumulator (can differ from list)
> When folding infinite lists, always foldr (guaranteed finite computation)
> Folds are also used to emulate traversal (accumulated or otherwise) to return one value e.g. maximum (treat acc as best-so-far)
- `scanr` and `scanl` are fold 2.0: They return each accumulator calculated (and hence are one longer than input list)
> Scanl order is chronological, scanr is reversed
- `filter f xs`
- `map f xs`
- `zipWith f xs ys` is essentially `map f (zip xs ys)`
- `flip` reverses order of *TWO* inputs, e.g. `f x y` to `f' y x = f x y`

Other:
- `odd`, `even`, `negate`
- `elem x xs` (checks membership)
- `const a b` (returns a) and `id` (identity for any argument)
- `reverse xs`
- `zip xs ys` 
- `compare` returns flags indicating `GT` `LT` or `EQ`
- `show` converts a Show data into its string equivalent (a casting of sort)
- `read :: Read a => String -> a` e.g. `read "1' == 1` is True (an 'eval' of sort)
- `error` takes a string and generates a runtime error
> Must type annotate if the output is not used

### Data.List
- `is<...>Of` for Subsequence, Suffix, Prefix and Infix
- `concat` removes one level of nested list
- `intersperse` concatenates a list with an element between
- `intercalate` concatenates a list with a LIST between
- `transpose` - works wonders with `map` for row/colwise operations for an array
- `lookup` a key in a list of tuples, returns a Maybe
- `sort xs`
- `group xs` splits list into sublists of adjacent matches
- `nub xs` removes duplicates
- `delete x xs` removes first occ. of `x`
- `xs \\ ys` removes as many `ys` values from `xs` as possible (i.e. think remove i xs for all i in ys)
- `isPrefixOf xs ys`

### GHC
- `:t f` or `:type f` gives type of `f` or any other haskell expr
- `:m + Data.List <other> <others...>` to import modules
- `:set -fwarn-incomplete-patterns` to warn on incomplate patterns
- `:info <TypeClass>`

# Questions

-Spacing convenntion for guards, wheres
- Mixing where/let for same sentence is bad practice
- Can't assign a set to a variable in Module 6...?
- What the hell is this:
```Haskell
  (x + 1) y = ...	-- Function binding, defines (+)
-- Equivalent to   (+) x 1 y = ...
```
- I'm finding the associativity of `<$>`, `<*>` a little tough. How does it wrap up 3 1-arity functions with a 3-arity operator?
- Where is only top-level function? meaning of top-level?
