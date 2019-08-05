# Haskell Techniques for COMP30020, COMP30026

### Author: Callum H

## Actual Files for Haskell

Structure base is as follows:
```Haskell
-- Imports
<Importation>

-- Code
main = do
    <monadic IO code>

-- Auxiliary Code
<function definitions>
```

### IO (WARNING: PRE-MONAD)

An IO action carries out an action with a side-effect (e.g. read/write to screen) and contains some return value.
- These actions are performed when given a name (specifically, they must be contained (eventually) in a `main` do) and the program executed ... or when used in GHCI

**Do** syntax glues together several I/O actions: it has an overall type `IO something` (albeit we don't write the type signature). 
- The last action of a `do` cannot be bound to a name; think of it as 'automagically binds the last expression to its result'
- EVERY line that isn't the last can be bound, e.g. `_ <- putStrLn "BLAH"`, although conventionally you don't unless binding is necessary
- `Let` bindings can be used `in`-less here for simplification
- **TIP**: Spend as little time as possible coding in **do**, because it isn't  pure

The `<-` operator executes and then extracts the 'boxed data' inside an `IO a` construct; thus `name = getLine` is an aliasing, not an extraction of output.

The `return` function 'wraps' a pure value into an IO action, i.e. `return () :: IO ()`. Thus you use return when a bogus, irrelevant return for `main` is needed. **It does NOT return a value in the imperative sense.**
- E.g. `a <- return "hell"` is equivalent to `let a = "hell"`, and `color <- getLine ; return color` equivalent to `getLine`

Examples:

```Haskell
main = do
    putStrLn "Enter a line, or hit enter to exit: "
    line <- getLine -- 'performs getLine and binds the result to <name>'; name :: String
    if null line
        then return ()
        else do
            putStrLn ("Okay, your line is: " ++ line)
            main
```

To use functions involving IO, it must be done in an 'IO environment' i.e. IO-based functions are only used/perform their IO side-effects within `do` blocks)
- E.g. `map print xs` does nothing but `sequence $ map print xs` does. 

**IO-Based Functions**:
- `putStrLn :: String -> IO ()` outputs a string to a line on execution WITHOUT the quotation marks
- `putStr` does not print a newline char post-print.
- `getLine :: IO String` reads a String
- `getChar`, `putChar`
- `print` is really `putStrLn . show`
- (from Control.Monad) `when` = `if <arg1> then <IO-action> else return ()`
- `getArgs` extracts the arguments fed to the program (NOT including itself) as a list of strings
- `getProgName` gets the name of the executed program

**IO is a applicative functor**:
- E.g. `line <- fmap reverse getLine` is very possible (converts `IO String` into `IO String`)
- E.g. `concated <- (++) <&> getLine <*> getLine`

### Compilation

```Terminal
> ghc -o hello hello.hs
> hello
OR
> runhaskell hello.hs
```

## Emulating Imperative Ideas

### Randomness (System.Random)

Typeclasses:
- `RandomGen` for sources of randomness
- `Random` for things that can take random values

Types:
- `StdGen` is a `RandomGen` output of `mkStdGen`

Functions:
- `random` Takes a random generator, outputs a tuple of a random value and a new generator (for randomness)
- `randoms` is an infinite sequence application of `random` to a generator; it does not return a new generator
- `mkStdGen Int` is a 'hash', takes in a seed and outputs a `StdGen` (a generator). Be careful to ensure when used that the output type is annotated.
- `getStdGen` is an IO action, asking the SYSTEM for a seed and returns an IO (global generator) which you bind for use
- Can use `newStdGen` to get a new one (also will change `getStdGen`'s output again)


### Statement Blocks

IDEA: Let many lets feed into each other. It 'feels' like imperative code but it's a complete expression.

```Haskell
let var1 = ... in
let var2 = ... in
output_expr
```

### Loops

```Haskell
loop1func (base, recursive) -- defines loop code, exit, entry
loop2func (base, recursive) -- defines loop code, exit, entry
...
let r1 = loo1func args. in -- LOOP
let .. = .. in (straight code)
let r2 = loop2func args. in -- LOOP
```

## Data Structures

### Trees

Binary: `data Tree a = Node a (Tree a) (Tree a) | Empty`, or more simply `data IntTree = Node Int IntTree IntTree | Empty`
- This is to say, a Tree can either be a node with two subtrees, OR empty.
- Ternary is obviously easy to create from this

#### Tree building example

Note that due to recursion, lists are traversed right to left below. For left to right insertion,reverse the list or implement with last and init.

```Haskell
-- BST data constructor
data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving Show

-- Inserts into a BST
insert :: Ord a => a -> (Tree a) -> (Tree a)
insert val Empty = Node val Empty Empty
insert val (Node x l r) 
    | val == x = (Node x l r)
    | val < x = Node x (insert val l) r
    | otherwise = Node x l (insert val r)
    
-- Builds a BST from a list of Ord
buildtree :: Ord a => [a] -> Tree a
buildtree [] = Empty
buildtree (x:xs) = insert x (buildtree xs)
```

#### Expression Tree

Think AST: It's a way of representing the grammar of a mathematical/character sentence and converting it into structured calculation.
- All leaves are numbers/base type
- All non-leaves are operations
- Each node is an 'expression' of type `data Expr = .. | ..`
- An evaluate function `evaluate :: Expr -> a` should be written that defines how to handle each operation. Then a tree can be evaluated.