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
    fs <*> xs = [f x | f <- fs, x <- xs]   -- Cartesian product as sizes could  unequal

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

**Monads** are an extension of Applicative Functors that also allows functions that take normal values and returns contextual values (a bind)

For review/comparison:
```Haskell
{-
<$> : Applies std functions to wrapped data
<*> : Applies wrapped (std function) to wrapped data
>>= : Applies wrapping function to wrapped data
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
    return :: a -> m a      -- IDENTICAL purpose to pure
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
```

Benefits:
- Bind allows extraction of underlying data without pattern matching (instantiation defines how to get it) whilst 'automagically' understanding the context, potential failures (like a 'Nothing') etc.

### Do Notation for Monads

Do notation is to streamline monadic notation. The following two are equivalent:

```Haskell

-- With monadic functions. Note that the brackets are un-necessary via precedence.
foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- Prettier version
foo2 :: Maybe String
foo2 = do
    x <- Just 3
    Nothing  -- Equivalent of _ <- Nothing, which really embodies a >>
    y <- Just "!"
    Just (show x ++ y)
```

So effectively `x <- Just 3` means `Just 3 >>= (<a function> \x -> <next line>)` (i.e. the `Just 3` is bound to the 'x' used in the rest of the computation, hence the name)
- Each line's RHS is the argument piped into the inner functions represented in the next line. Therefore the last line must be an expression as the output of the innermost lambda!
- So it's conditionally handling all cases AND excellent piping in a clear, readable manner!
- Thus, monads are great for scenarios where operations are chained, but multiple cases need to be handled. The case handling is decoupled from the chain. Applicative on the other hand can only apply, not systemically handle cases.

Pattern matching is certainly possible as the LHS **is the argument** to the next inner lambda. 

The **error** function exists to handle what to do in a pipe scenario when the pattern match fails in a **do**. The error allows for contextual messaging, or just a default value based on the context.

TODO: Try and do dos with the Juggler. Remember, write as a sequence FIRST, then convert.

