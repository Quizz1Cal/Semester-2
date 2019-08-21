{-
:Author: Callum H
:Title: N-Queens Solver for NxN board. 
:Purpose: 
  Given a desired dimension N, the program generates all solutions.
  There is no attempt to eliminate rotationally similar solutions.
  The option to print to the screen is provided.
-}

-- Use for reducing solution space
import Data.List (sort, nub)

{- Type synonyms for main structures. 
   Boards have indices 1 to N along row and column.
-}
type Queen = (Int,Int)
type Board = [Queen]

-- To print all solutions for an N-Queens problem
printAll :: Int -> IO ()
printAll n = sequence_ $ map (printBoard n) (solver n) 

-- Printing wrapper function, uses printPiece for piecewise representation
printBoard :: Int -> Board -> IO ()
printBoard _ [] = return ()
printBoard n board = sequence_ [printPiece n (x,y) board | x <- [1..n], y <- [1..n]]

-- Helper function, prints an individual piece and appropriate newline/space
printPiece :: Int -> Queen -> Board -> IO ()
printPiece n (x,y) board =
  putStr (symbol:separator)
  where 
    symbol = 
      if (x,y) `elem` board
      then 'Q' 
      else '.'
    separator =
      if (y == n)
      then 
        if (x == n)
        then "\n\n" 
        else "\n"
      else " "

-- Retrieves solutions to the N-Queens problem with board size (NxN)
solver :: Int -> [Board]
solver n = nub $ (iterate (>>= allAdditions n) (return [])) !! n

{- Given a (valid) board with m queens, 
   finds all m+1 boards using these previous that are valid.
-}
allAdditions :: Int -> Board -> [Board]
allAdditions n board = [
  insert (x,y) board | 
  x <- [1..n]
  , y <- [1..n]
  , isValid n (x,y) board
  ]

{- Inserts a queen into a board. Sorts for consistency.
   Assumes board is valid.
-}
insert :: Queen -> Board -> Board
insert queen board = sort $ queen : board

{- Checks if inserting the given queen yields a valid board.
   Assumes given board is already valid
-}
isValid :: Int -> Queen -> Board -> Bool
isValid n queen board = 
  -- Check for Valid index; and that no row/col/diagonal overlaps
  (1,1) <= queen && queen <= (n,n)
  && not (fst queen `elem` (map fst board))
  && not (snd queen `elem` (map snd board))
  && and [not (isDiag r s) | r <- newBoard, s <- newBoard, r /= s]
  where newBoard = insert queen board

-- Checks if two queens are diagonally in line
isDiag :: Queen -> Queen -> Bool
isDiag (x,y) (a,b) = (x-a) == (y-b) || (a-x) == (y-b)
