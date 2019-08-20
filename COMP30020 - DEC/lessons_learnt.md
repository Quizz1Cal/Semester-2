## Questions
- Q: when say Integer, you want Int or ... what?}
- Q: to clarify, when i do `data Stuff a = ... deriving Eq`, we DONT class-constrain a?

## Workshop 2
- When using abstract types, need to enforce class
- Always ask if all possible values for the data struct are sensible
- if they want `myElem` don't use `elem`.
- `x:REST` is better than `REST++[x]` unless you're adding to the end ... even then it's complex
- You can't use `succ` on the last member of an enumerable, you get error. Similar for `pred`

## Workshop 3
- error morphs into its surroundings to become the same data type.
- Should use errors more frequently

## Workshop 11 ALT
1. To use >>=, MUST be m a -> a -> m b -> m b. I was doing IO Tree -> Tree -> IO (Int, Tree) i.e. 3 different types.
2. Use STATE next time to track the n
3. Investigate complete isolation of the tree/IO (but I'm p sure this breaches the 'encapsulation' principle of monads)
4. Need `do` every time you want to use <-
5. I do still think it's possible to just put the 's inside the traverser and through partials, use >>=

### Idiom for While

IDIOM for while loops. Use a tuple, etc. where many args are needed
```Haskell
whiler x = 
  if cond x
  then whiler $ next x
  else final x
```