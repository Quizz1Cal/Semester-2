-- Workshop 1 Haskell questions

import qualified Data.List as DL

-- Question 3
paths :: Integer -> Integer -> Integer
paths x y = case (x,y) of
    (1,_) -> 1
    (_,1) -> 1
    (x,y) -> paths (x-1) y + paths x (y-1)

-- Question 4
type Domino = (String, String)

-- Get left/right string sequences
getLeft :: [Domino] -> String
getLeft ds = foldl (\str dom -> str++(fst dom)) "" ds

getRight :: [Domino] -> String
getRight ds = foldl (\str dom -> str++(snd dom)) "" ds

-- Given current sequences, attempts to add domino to each (does keep original)
nextseq :: [[Domino]] -> Domino -> [[Domino]]
nextseq [] d = [[d]]
nextseq seqs d = 
    [seq | seq <- map (++[d]) seqs
        , let left = getLeft seq
        , let right = getRight seq
        , DL.isPrefixOf left right || DL.isPrefixOf right left]

-- Next step
expandseq :: [Domino] -> [[Domino]] -> [[Domino]]
expandseq ds seqs = 
    let newseqs = map (nextseq seqs) ds
    in foldr (++) [] newseqs

-- Calculates the nth generation
expand :: [Domino] -> Int -> [[Domino]]
expand ds n 
    | n <= 0 = []
    | otherwise = next $ expand ds (n-1)
    where next = expandseq ds

doms = [("b","ca"),("a","ab"),("ca","a"),("abc","c")]

