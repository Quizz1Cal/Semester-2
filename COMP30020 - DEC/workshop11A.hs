-- Wk 11 ALT

-- Tree. NO <> YES. If you get to a Leaf, that's when you ask "is it a ..."
type Animal = String
type Question = String
type Answer = String
data Tree = Finished | Leaf Animal | Decision Question Tree Tree
    deriving (Eq, Show)

{- Note: while Decision Q Finished Finished is possible (bad), 
    by nature of code it would not happen.
-}

-- Main function
main :: IO ()
main = do
  start_game_prompt
  (_,tree) <- (iterate (>>= traverseWithCount) (return initial)) !! 20
  return ()
  where initial = (0, Leaf "Penguin")

-- Tree traversal
{-
Traverses the given tree based on user IO. 
Computes the number of guesses if success, or for failure.
-}
traverseWithCount :: (Int, Tree) -> IO (Int, Tree)
traverseWithCount (n,tree) = 
  case tree of
    Finished -> 
      -- Game has already finished, ignore
      return (n, Finished)
    Decision q lt rt -> do 
      ans <- question_prompt q
      if (ans == "NO")
      then do
        (newn,newlt) <- traverseWithCount (n,lt)
        return (
          if newlt == Finished 
          then (newn, Finished)
          else (newn, Decision q newlt rt)
          )
      else do
        (newn,newrt) <- traverseWithCount (n,rt)
        return (
          if newrt == Finished 
          then (newn, Finished)
          else (newn, Decision q lt newrt)
          )
    Leaf animal -> do 
      ans <- leaf_prompt animal
      if (ans == "NO")
      then do
        -- Gets more info from user, updates tree for next round
        new_data <- new_data_prompt animal
        start_game_prompt
        return (n+1, insert tree new_data)
      else do
        -- Game over - pass empty tree to kill future calls
        end_game_prompt n
        return (n, Finished)

{-
Inserts a new info tuple where some conflict occurred.
I.e. Was finally asked if it's a penguin, but user said so.
This converts Leaf Penguin to a branch into Penguin or the new item.
-}
insert :: Tree -> (Animal, Question, Answer) -> Tree
insert conflict (newAnimal, q, newAns) = 
  Decision q left right
  where 
    (left, right) = 
      if (newAns == "NO")
      then (Leaf newAnimal, conflict)
      else (conflict, Leaf newAnimal)
        

-- IO operations. 
{-
Note how the output has to be piped as an IO OUTPUT_TYPE, 
cause that's how do works, it outputs the Monadic structure
-}
new_data_prompt :: String -> IO (Animal, Question, Answer)
new_data_prompt prior = 
  do 
    putStr $ "Sorry! What was your animal?\t"
    animal <- getLine
    putStr $ "Okay - now write a question to\ndiffer it from a " ++ prior ++ ":\t"
    question <- getLine
    putStr $ "Is this true for your animal?\t"
    answer <- getLine
    putStr $ "Thanks!\n\n|| NEXT ROUND ||\n\n"
    return (animal, question, answer)

start_game_prompt :: IO ()
start_game_prompt = do
  putStr "Think of an animal. Hit return when ready. "
  getLine
  return ()

end_game_prompt :: Int -> IO ()
end_game_prompt tries = 
  putStrLn $ "Haha! I guessed it with " ++ str_tries ++ "!"
  where 
    str_tries = 
      if tries == 1
      then "1 fail"
      else (show tries) ++ " fails"

question_prompt :: String -> IO String
question_prompt q =
  do putStr $ "QUESTION > " ++ q ++ "\t"
     getLine
 
leaf_prompt :: String -> IO String
leaf_prompt animal = 
  do putStr $ "QUESTION > Is it a " ++ animal ++ "?\t"
     getLine
      
-- Example
example = (5,
  Decision "Wings?" 
    (Decision "Can fly?" 
      (Leaf "Penguin") 
      (Decision "Extinct?" 
        (Leaf "Eagle") 
        (Leaf "Pterodactyl")
      )
    ) 
    (Decision "Stripes?" 
      (Leaf "Lion") 
      (Decision "Herbivore?" 
        (Leaf "Tiger") 
        (Leaf "Zebra")
      )
    )
  )