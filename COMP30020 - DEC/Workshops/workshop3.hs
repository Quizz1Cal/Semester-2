-- Hello there
import Data.List (sort)

-- Q1
{- (a) sounds tempting but so does (c).
(c) is better since it would store the additional STRUCTURE information.
-}

-- Q2
ftoc :: Fractional a => a -> a
ftoc f = (5/9) * (f-32)
-- Clearly doubles are an instance of Fractional a,
-- and so can mess about with other fractionals.

-- Q3
quadRoots :: (Floating a, Ord a) => a -> a -> a -> [a]
quadRoots a b c
    | disc < 0 = error "No real solutions"
    | disc == 0 = [h]
    | otherwise = [h-width, h+width]
    where disc = b^2 - 4*a*c
          width = sqrt(disc)/(2*a)
          h = (-b) / (2*a)

-- Q4
merger :: Ord a => [a] -> [a] -> [a]
merger (x:xs) (y:ys)
  | x <= y = x : merger xs (y:ys)
  | otherwise = y : merger (x:xs) ys
merger xs ys = xs ++ ys

-- Q5
-- HAD AN ERROR: Couldn't exclude the actual pivot from the split!
quicksort :: Ord a => [a] -> [a]
quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
    where left = filter (<=x) xs
          right = filter (>x) xs
quicksort _ = []

-- Q6
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
    deriving (Eq, Show)

same_shape :: Tree a b -> Tree c d -> Bool
same_shape (Node x y l1 r1) (Node q r l2 r2)
    = same_shape l1 l2 && same_shape r1 r2
same_shape Leaf Leaf = True
same_shape _ _ = False

tree1 = Node 2 1 (Node 2 1 Leaf Leaf) (Leaf)
tree2 = Node 'c' 'd' Leaf (Node 'c' 'f' Leaf Leaf)
tree3 = Node 'a' 'b' Leaf (Node 'y' 'd' Leaf Leaf)

-- Q7
data Expression
    = Var Variable
    | Num Integer
    | Plus Expression Expression
    | Minus Expression Expression
    | Times Expression Expression
    | Div Expression Expression
data Variable = A | B

-- eval :: Integer -> Integer -> Expression -> Integer
eval a b (Num c) = c
eval a b (Var A) = a
eval a b (Var B) = b
eval a b (Plus exp1 exp2) = eval a b exp1 + eval a b exp2
eval a b (Minus exp1 exp2) = eval a b exp1 - eval a b exp2
eval a b (Times exp1 exp2) = eval a b exp1 * eval a b exp2
eval a b (Div exp1 exp2) = div (eval a b exp1) (eval a b exp2)
