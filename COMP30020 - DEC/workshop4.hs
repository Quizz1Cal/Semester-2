-- Workshop 4 baby

-- Q1
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

{-
Sort using a BST.
1. Insert one a time into some BSt.
-}

-- BST_sorter :: Ord a => [a] -> [a]
-- BST_sorter [] = []
-- BST_sorter xs = 

-- Receives list of so far, current tree
-- IDEA: You get the best so far.
-- YOUR JOB: Get the smallest, add to FRONT

bst_sorter :: Ord a => [a] -> [a]
bst_sorter [] = []
bst_sorter xs = inOrder $ construct_tree xs
  where 
    inOrder Leaf = []
    inOrder (Node v l r) = inOrder l ++ v : inOrder r    

construct_tree :: Ord a => [a] -> Tree a
construct_tree xs = foldr insert_tree Leaf xs

-- Inserts into a BST. <= , > structure.
insert_tree :: Ord a => a -> Tree a -> Tree a
insert_tree x tree = 
    case tree of
        Leaf -> Node x Leaf Leaf
        Node val lt rt -> 
            if x <= val
            then Node val (insert_tree x lt) rt
            else Node val lt $ insert_tree x rt

tree = Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf (Node 7 Leaf Leaf))

-- Q2
{-
Assume ALL lists are non-empty
Assume inner lists ALL of same length
Iteration:
-- extract heads --> one item
-- ask for rest of it (post-extraction)

-}
transpose :: [[a]] -> [[a]]
transpose xs = 
    map (takenth xs) [0..(n-1)]
    where takenth xs n = map (!!n) xs
          n = length $ head xs

-- Q3, Part 1
informer :: Num a => [a] -> (Int,a,a)
informer xs =
    (length xs, sum xs, sum $ map (^2) xs)

-- Q3, part 2
singleformer :: Num a => [a] -> (Int, a, a)
singleformer xs = foldr tupler (0,0,0) $ map (\x -> (1,x,x^2)) xs
    where tupler (x,y,z) (a,b,c) = (x+a,y+b,z+c)

testformer :: Num a => [a] -> (a,a,a)
testformer xs = 
    let (x:y:z:[]) = foldr (zipWith (+)) [0,0,0] $ map (\x -> [1,x,x^2]) xs
    in (x,y,z)

-- Better
stats2 [] = (0,0,0)
stats2 (n:ns) =
  let (l,s,sq) = stats2 ns
  in (l+1, s+n, sq+n*n)