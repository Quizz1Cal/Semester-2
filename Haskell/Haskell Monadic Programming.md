# Haskell Notes on Functors, Monoids, Monadic Programming

### Author: Callum H

## Functors

**Functors** are a typeclass for things that can be mapped (i.e. a function applied to some underlying data/thing) over with `fmap`; lists are an instance. Things that are not a member of the `Functor` typeclass cannot use `fmap`
- Typically used to hide implementation of some data type, whilst facilitating the ability to operate on it/operate with it
- A data type has to have a type constructor of kind `* -> *` (maps a single concrete type to a concrete type)
> Therefore while `Either a b` cannot be made a Functor, `Either a` can.

**Functor over numbers** is to say a functor with numbers within it. For example, `fmap (*2)` works on functors over numbers.

**WARNING**: Do not consider `fmap` to be equivalent to `map`. Map works on lists because that is how `fmap` is defined for lists. `fmap` only operates on the underlying data; for a list, that is all the entries, for a Maybe, that is just the data stored in a Just construct.

**WARNING FROM LEARN YOU A HASKELL**: *"Many times the box analogy is used to help you get some intuition for how functors work, and later, we'll probably use the same analogy for applicative functors and monads... (but) for some functors the box analogy has to be stretched really thin to still hold some truth. A more correct term for what a functor is would be **computational context**. The context might be that the computation can have a value or it might have failed (Maybe and Either a) or that there might be more values (lists), stuff like that."*

### Examples
```Haskell

-- Note the variable is a type constructor and NOT a concrete Type
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map 

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

-- Functions are also functors, as they 'contain' their result. The application operator ((->) r)
instance Functor ((->) r) where  
  fmap = (.)  
```

### Functor Laws
1. `fmap id = id` (Identity)
2. `fmap (f . g) = fmap f . fmap g` (Composition)

An example of a functor disobeying the laws would be one that updates a counter each mapping - clearly, mapping the `id` function would disagree with `id`.

Other functor examples:
- `fmap (+3) (*3) $ 5` yields `3 + 3*5` (because functions are functors)

## Applicative Functors

Applicative functors are a subclass of Functors that facilitate the ability to apply functor-wrapped operations to other functors (i.e. hidden operators and hidden operands).
- Located in `Control.Applicative`

### Examples
```Haskell
class (Functor f) => Applicative f where  
  pure :: a -> f a  -- How to wrap an expression into a functor
  (<*>) :: f (a -> b) -> f a -> f b   -- How to apply a functor operation to a functor

-- Adding a member
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> x = fmap f x

-- For lists
instance Applicative [] where  
  pure x = [x]  
  fs <*> xs = [f x | f <- fs, x <- xs]   -- Cartesian product as sizes could be unequal

-- for IO
instance Applicative IO where
  pure = return
  a <*> b = do
    f <- a   -- (unpack fxn in a)
    x <- b   -- (unpack data in b)
    return (f x) -- (repack post-map)

-- ((->) r) (function application)
instance Applicative ((->) r) where
  pure x  = (\_ -> x)  -- A pure function is the 'constant'
  f <*> g = \x -> f x (g x) -- 

-- Infix equivalent for fmap in the same module
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x  
```

Example uses:
- `pure (+) <*> Just 3 <*> Just 5` yields `Just 8` thanks to partial application
- `pure f <*> x` is equivalent to `fmap f x`
- `f <&> X <*> Y <*> Z = (fmap f X) <*> Y <*> Z` where `x,y,z` are functor instances (`fmap f x` is a partial), and is analogous to `f x y z` where `x,y,z` are normal values

Functions are applicative, so a N x 1-arity functions can operated on by an N-arity function:
- `f = (+) <$> (+3)` becomes `f = (+) . (+3)` and so `g = f <*> (*100)` becomes `g = \x -> ((+) . (+3)) x ((*100) x)` (curiously the `(+3)` applies only to the first argument) and so `g 5 = 5+3 + 5*100 = 508`. 
- OR `(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5`

Flaws/downsides:
- Cannot bode well with a stream of function applications to the same object. Above are examples of great composition, but at most one per argument. For effective, readable streamed application, go to **Monads**.

### Applicative Functor Laws
1. `pure f <*> x = fmap f x` (indifferent use of fmap)
2. `pure id <*> v = v` (a consequence of (4) really)
3. `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)` (...)
4. `pure f <*> pure x = pure (f x)` (indifferent use of pure)
5. `u <*> pure y = pure ($ y) <*> u` (...)

## Monoids

A **monoid** is a type and and associative binary function (takes 2 args, bracketing irrelevant) with some identity value.
- E.g. `+`, `++`, `.` are effectively monoids with `Num, List, Function` with identities `0, [], 1, id`
- **Located in `Data.Monoid`**

Examples:
```Haskell

-- class definition. Note m is a concrete type
class Monoid m where
  mempty :: m     -- a polymorphic constant representing the identity value
  mappend :: m -> m -> m      -- the binary function 
  mconcat :: [m] -> m     -- Reduces a list of monoid vals, default impxn.
  mconcat = foldr mappend mempty          -- Can be overriden if needed

-- lists (work for any a). Note [] below wouldn't work, since that's not a type.
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

Where one type has monoidic properties with many functions, `newtype` is used to create wrapper types, and then those wrapper types are made Monoids
- E.g. For numbers, `newtype Product a = Product {getProduct :: a}` and similarly defined `Sum a`. 
> So `mappend 3 9` won't work (because there are many options for the a. b. function) but `mappend (Product 3) (Product 9) == Product 27`
- E.g. for Bool, `newtype Any` and `newtype All` as `&&` and `||` are both a. b. functions w. r. t. Bools
- More compact comparisons: for `Ordering`, you can do `(compare length x length y) 'mappend' (compare x y)`
    - If the first compare is non-equal the decision is made
    - If equal, the second comparator takes care of the rest. This because `mappend GT/LT _ == <same>` but `mappend EQ _ = EQ` (`mempty == EQ`)

When manipulating monoids, use associated getter functions for extraction.

### Monoid Laws
1. `mempty \`mappend\` x = x` (identity works)
2. `x \`mappend\` mempty = x` (identity works regardless of order)
3. `(x \`mappend\` y) \`mappend\` z = x \`mappend\` (y \`mappend\` z)` (associativity of any item)

### Folding with Monoids

**Foldable** typeclass is in `Data.Foldable` (best imported qualified as (for the below examples) `F`)
- Offers new `F.foldr, F.foldl, F.foldr1, F.foldl1` to take any Foldable (not just lists)

E.g. `foldr (*) 1 [1..3] == 6`, `F.Foldl (+) 2 (Just 9) == 11` (so really for basic data types its fairly indifferent to `<$>`)

To instantiate as a member of `Foldable`, best to implement `foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m` (which is used to define `F.foldr` and `F.foldl`).
> Argument 1: Function that extracts a monoid value from the data's hidden type `a`. This function will map all structures to monoid values, then use `mappend` to fold them into the final answer.
> Argument 2: (Nested) data structure with hidden type `a`
> Output: Monoid value

**TIP**: When defining foldMap, you define 'how to zip the structure of monoid values together'. When using foldMap, you define 'how to convert the type into a monoid value'. Don't confuse the two!

Examples:

```Haskell

{- In-order fold for Tree data type.
   The actual monoid and tree data is irrelevant - this only describes HOW to handle folding the tree, not the actual monoid operations.
-}
instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r

{- Excellent boolean test. Note the need to use Any/All because Bool is not a monoid due to having two a. b. functions -}
getAny $ F.foldMap (\x -> Any $ x == 3) testTree

-- Simpler examples
F.foldl (+) 0 testTree
F.foldMap (\x -> [x]) testTree  -- Converts tree into a list
```

## Monads

**Monads** are an extension of Applicative Functors that also allows functions that take normal values and returns contextual values (a bind). *Think of them as a type constructor representing a computation; programmable semicolons*.

*Philosophy*: Interpret monadic functions as returning TWO values:
- The value of type `t'
- A description of an operation (e.g. IO)
- return is just 'do nothing IO'
- >>= is 'do these things in order'

*Complete haskell programs* have a main function `main :: IO ()`

For review/comparison:
```Haskell
{-
<$> : Applies std functions to wrapped data
<*> : Applies wrapped (std function) to wrapped data
>>= : Applies wrapping function to wrapped data
=<< : (think `flip (>>=)`) Takes wrapped data and applies wrapping function
>>  : Forces right to be output, unless >>= says otherwise (e.g. Nothing >> Just 3 yields Nothing and not Just 3)
}

(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b -- m is a Applicative Functor that can handle this operation.
(>>) :: (Monad m) => m a -> m b -> m b
x >> y = x >>= \_ -> y
(fail) :: String -> m a
fail msg = error msg
```

Examples
```Haskell

{- Class definition. 
   Note the absence of the typeclass constraint; but it is still true that Monads are Applicative Functors.
-}
class Monad m where
  return :: a -> m a      -- IDENTICAL purpose to pure; identity function
  (>>=) :: m a -> (a -> m b) -> m b       -- Bind
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  fail :: String -> m a
  fail msg = error msg

-- Maybe
instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x     -- Remember the function outputs a Maybe
  fail _ = Nothing

-- Lists (and equivalent statements)
[1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n,ch)
[1,2] >>= (\n -> ['a', 'b'] >>= (\ch -> return (n, ch)))
[(n,ch) | n <- [1, 2], ch <- ['a', b']]
do 
  n <- [1,2]
  ch <- ['a', 'b']
  return (n,ch)
```

Benefits:
- Bind allows extraction of underlying data without pattern matching (instantiation defines how to get it) whilst 'automagically' understanding the context, potential failures (like a 'Nothing') etc.

### Monadic Composition

The composition operator(s) (`Control.Monad`) are defined as
```Haskell
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f)

-- Informally: (>=>) = flip <=<.
```

Note that, if you use composition/bind operators pointing in the same 
direction, you read and apply the stream linearly (see below).

```Haskell
-- Example
pred2 = Just . pred . pred
negater = \y -> Just $ -y

(pred2 <=< negater) =<< Just 5   -- Just (-7). Read right-left
Just 5 >>= (negater >=> pred2)   -- Just (-7). In order of application!
pred . pred . negate $ 5         -- (-7)

(negater <=< pred2) =<< Just 5   -- Just (-3)
negate . pred . pred $ 5         -- (-3)
```

### Monad Laws

1. Left Identity: `return x >>= f` is equivalent to `f x` (i.e. the process of packing, then unpacking shouldn't mutate the data)
    - Therefore writing a `return <midresult>` mid-computation is fine since the `<midresult>` will still pass through it.
2. Right Identity: `m >>= return` is equivalent to `m` (i.e. the process of unpacking then packing shouldn't mutate the data)
3. Associativity: `(m >>= f) >>= g` is equivalent to `m >>= (\x -> f x >>= g)`
    - Based off understanding that `m@(<context> x) >>= f = f x`
    Note: does not conflict with list comprehensions since `g` is not independent of `x` there

Re-expressed with composition operators:
1. `f <=< return` is equal to `f` (wraps, dewraps, then applies `f`)
2. `return <=< f` is equal to `f` (applies `f`, dewraps then wraps)
3. `f <=< (g <=< h)` is equal to `(f <=< g) <=< h`

### Do Notation for Monads

Do notation is to streamline monadic notation. The following two are equivalent:

```Haskell

-- With monadic functions. Note that the brackets are un-necessary via precedence.
foo :: Maybe String
foo = 
  Just 3   
  >>= 
  \x ->
  Just "!" 
  >>= 
  \y ->
  Just (show x ++ y)))

-- Prettier version
foo2 :: Maybe String
foo2 = do
    x <- Just 3
    y <- Just "!"
    let k = "Cool "
    Just (k ++ show x ++ y)
```
> Note: writing `Nothing` or `_ <- Nothing` in `foo2` would equal a `>> Nothing` in `foo`

So effectively `x <- Just 3` means `Just 3 >>= (<a function> \x -> <next line>)` (i.e. the `Just 3` is bound to the 'x' used in the rest of the computation, hence the name)
- Each line's RHS is the argument piped into the inner functions represented in the next line. Therefore the last line must be an expression as the output of the innermost lambda!
- So it's conditionally handling all cases AND excellent piping in a clear, readable manner!
- Thus, monads are great for scenarios where operations are chained, but multiple cases need to be handled. The case handling is decoupled from the chain. Applicative on the other hand can only apply, not systemically handle cases.

Pattern matching is certainly possible as the LHS **is the argument** to the next inner lambda. 

The **error** function exists to handle what to do in a pipe scenario when the pattern match fails in a **do**. The error allows for contextual messaging, or just a default value based on the context.

### MonadPlus and Monadic Filtering

Located in `Control.Monad.Plus`, MonadPlus is a class for monads that also act as monoids. 

```Haskell
class Monad m => MonadPlus m where
  mzero :: m a                   -- synonym for mempty
  mplus :: m a -> m a -> m a     -- synonym for mappend

-- List example
instance MonadPlus [] where
  mzero = []
  mplus = (++)
```

Using the function `guard :: (MonadPlus m) => Bool -> m ()`, we can add filters to `do` sequences for more than just lists.
- E.g. `guard (5 > 2) :: Maybe ()` outputs `Just ()`
- E.g. `guard (False) :: Maybe ()` gives `Nothing`. 
- In a chain of `>>=`, the former along with `>>` is an identity (effectively), the latter a nullification
    - E.g. `guard (True) >> return "cool" :: [String]`
    - All it requires is that `()` can be placed into the monad's context

```Haskell
gchi > guard (True) >> return "cool" :: [String]   -- Or even Maybe String
["cool"]
```

To filter with `guard` in a do, simple write `guard (<some test>)` as one of the lines

### Built-In Monadic Types

#### Reader i.e. Why Monads > Applicatives

```Haskell

-- Functions are Monads
-- Note: the f below is type a -> b -> r (takes in output of first func and input f first func)
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w   -- Gets the x-value 'w', gets h w, applies f to everything

-- The beauty of functions being monads
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)  -- You pretend you already know the values of a and b

-- addStuff 3 is 19
-- So is (+) <$> (*2) <*> (+10) $ 3
```

#### Writer

The `writer` monad captures a value and some associated monoid in a tuple `(<value>, <monoid log>)`. An `>>= f` operand will apply `f` to the value, but also call `mappend` to fold the prior log and log of `f <value>`. So the side effect here is the folding of the Monoidic value. 

```Haskell
newtype Writer w a = Writer { runWriter :: (a, w) }

-- Instantiation
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
```

Examples:
```
-- Using to track use of numbers
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3        -- The 'context' is (Writer w), even though 'runWriter' does otherwise
    b <- logNumber 5  
    return (a*b)  

-- Call
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])  
```  

The `tell` function returns a dummy a value, but `mappend` the argument to the log - basically a `return` with side effect.

Thus to add logging to programs is easy: just wrap outputs in a `do`, use `tell` to indicate the output, and use `return <intention>`. Then to unpack it, use `mapM_ putStrLn $ snd` on the final output for pretty print.


### State Monad

Found in `Control.Monad.State.Lazy`, defines a State, which implicitly stores a value and some state. Stateful computations (functions that, given a state, compute a value and a new state) are the functions that interact with bind.

**TIP**: A state s a is better thought of as a 'unevaluated state' than an actual state object. Will it be a state? Yes. But have you applied the prior state yet? No.

```Haskell
-- Idea of implementation
newtype State s a = State { runState :: s -> (a,s) }  

instance Monad (State s) where  
  return x = State $ \s -> (x,s)  
  (State h) >>= f = State $ \s -> 
    let (a, newState) = h s  
        (State g) = f a  
    in  g newState 

Idea behind bind:
___________________________________
| (input) >[H]> (a) >F> ([G])     |
|             > (state) ----->[G]>| 

Brackets: inputs/outputs
[]: A state's function
{}: The function being bound: given a value, creates THE desired state function.
```

Example use:
```Haskell
-- Implementing a Stack. Note that pop/push are 'constants' in the foreground but (in the side-effect world) they are actually implicit functions that return a stack.

-- Pop does nothing in foreground, so it's a constant Stack by our viewpoint. If it returned it's popped value, then it would be differently implemented.
pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs) 

-- Will perform these actions when given a Stack. For now, just a complex composition.
stackManip = do
    push 3
    a <- pop
    pop
```

State Monad Notes:
- Functions of form `val -> s -> (val,s)` are perfect for composition
- A state is a FUNCTION. Yes it's an 'object' but that is hard to interpret.
> If you have a `(State s a)`, you have a state generator.

> If you have an `a` you have an underlying data field

> If you have an `s` you have an underlying state.

> If you have an `a -> (State s a)` you have a state-based computation (`>>=`) 

Common State functions:
- `runState :: State s a -> s -> (a, s)` extracts the function from a State object
- `return :: a -> (s -> (s, a))` converts a raw data into a State object

The return is a functional constant of sorts - it will pass the `x` (as you'd expect) with the minimum context (whatever state you pass, since you don't interact with it here). 

The **MonadState** typeclass (`Control.Monad.State`) also provides the below:

```Haskell
get = State $ \s -> (s,s)
put newState = State $ \s -> ((), newState)
```

which allows fetching and overriding of the states as such:
```Haskell
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  
```

#### Randomness (System.Random)

Typeclasses:
- `RandomGen` for sources of randomness
- `Random` for things that can take random values

Types:
- `StdGen` is a `RandomGen` output of `mkStdGen`

Functions:
- `random :: (RandomGen g, Random a) => g -> (a, g)` takes a random generator, outputs a tuple of a random value and a new generator (for randomness)
> This is a stateful computation
- `randoms` is an infinite sequence application of `random` to a generator; it does not return a new generator
- `mkStdGen Int` is a 'hash', takes in a seed and outputs a `StdGen` (a random generator). Be careful to ensure when used that the output type is annotated.
- `getStdGen` is an IO action, asking the SYSTEM for a seed and returns an IO (global generator) which you bind for use
- Can use `newStdGen` to get a new one (also will change `getStdGen`'s output again)

Monad application:

```Haskell
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random   -- random is the perfect function!

-- Coin tossing. Remember the type declaration is key - it casts the random number.
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)  
```
