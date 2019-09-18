-- Q1
product xs = foldl (*) 1 xs
all_pos = filter (>0)
some_not_pos = Just $ filter (<=0)

len' :: Num a => [a] -> Int
len' = foldr (\_ acc -> acc+1) 0
-- Performance of len is pretty dysmal. Faster ways?
-- Thought the pointfree style was p cool

-- TODO: Undestand how (+2) >>= (+) works

{-
(>>=) :: m a -> (a -> m b) -> m b
return (+2) == (\_ -> (+2))
(>>=) f acc = (\x -> acc (f x) x)

-}

-- Balancing is freaking hard

-- Q18. Idea for a cord struct with unambiguous emptiness.
{-
NO/EMPTY TREE: Nothing
ONE ELEM: Leaf x
TWO ELEMS: Branch (Leaf x) (Leaf y)
THREE ELEMS: Branch (Branch () ()) (Leaf y) etc.
<=, >
-}

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Ord, Show)
type Cord a = Maybe (Tree a)

-- LESSON: It needed a LIST OF STRING since that's what ++ does
-- Q20
issubstr :: Eq a => [a] -> [a] -> Bool
issubstr key src = 
  let suffixs = foldr (\x acc -> [x:(acc!!0)]++acc) [[]] src
      test = \x -> take (length key) x == key
  in or $ map test suffixs