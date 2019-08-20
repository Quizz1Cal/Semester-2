{-
N-Queens solver. Use printAll or getter $ (N) to generate answers.
-}

import Data.List (sort, nub)
-- Use for reducing solution space

-- Type synonyms. Note chose to index from 1.
type Queen = (Int,Int)
type Board = [Queen]

-- To print all solutions for an N-Queens problem
printAll n = sequence_ $ map (printer n) (getter n) 

-- Prints a board
printer :: Int -> Board -> IO ()
printer _ [] = return ()
printer n board = sequence_ [printPiece n (x,y) board | x <- [1..n], y <- [1..n]]

-- Help function for board printing
printPiece n (x,y) board =
  putStr (flag:line)
  where 
    flag = 
      if (x,y) `elem` board
      then 'Q' 
      else '.'
    line =
      if (y==n)
      then 
        if (x==n)
        then "\n\n"
        else "\n"
      else " "

-- Retrieves solutions to the N-Queens problem with board size (NxN)
getter :: Int -> [Board]
getter n = nub $ (iterate (>>= next n) (return [])) !! n

-- Given a (valid) board with m queens, finds all m+1 boards using these previous that are valid
next :: Int -> Board -> [Board]
next n board = [
  insert (x,y) board | 
  x <- [1..n]
  , y <- [1..n]
  , isValid n (x,y) board
  ]

-- Inserts a queen into a board. No validity check
insert :: Queen -> Board -> Board
insert queen board = sort $ [queen] ++ board

-- Checks if a queen to be inserted yields a valid board
isValid :: Int -> Queen -> Board -> Bool
isValid n queen board = 
  (1,1) <= queen && queen <= (n,n)
  && not (fst queen `elem` (map fst board))
  && not (snd queen `elem` (map snd board))
  && and [not (isDiag r s) | r <- newBoard, s <- newBoard, r /= s]
  where newBoard = insert queen board

-- Checks if two queens are diagonally in line
isDiag :: Queen -> Queen -> Bool
isDiag (x,y) (a,b) = (x-a) == (y-b) || (a-x) == (y-b)
