{-
:Author: Callum H
:Title: Workshop11ALT - Animals game
:Purpose: 
  Simulates a classic Animals game. The program starts knowing only
  of penguins. Over time as the user thinks and the program attempts to
  guess, it request more animal names and questions to distinguish them.
  The game ends when the programs successfully guesses a thought animal.
:future:
  Could build a thorough dictionary of questions and import for a more
  interesting experience.
-}


import System.IO

type Animal = String
type Question = String
type Answer = String
-- InfoTuple stores the new info required to extend a tree at each round.
type InfoTuple = (Animal, Question, Answer)

{- Tree structures questions, with animal guesses as leaves.
   LEFT is NO, RIGHT is YES. 
   Finished flags game over.
-}
data Tree = Finished | Leaf Animal | Decision Question Tree Tree
    deriving (Eq, Show)

-- Constants
no = "NO" :: Question

-- Main function
main :: IO ()
main = do
  start_round_prompt
  (_,tree) <- (iterate (>>= traverseWithCount) (return initial)) !! 20
  return ()
  where initial = (0, Leaf "Penguin")

{- Traverses the given tree based on user input.
   Computes the number of guesses n by computer.
-}
traverseWithCount :: (Int, Tree) -> IO (Int, Tree)
traverseWithCount (n,tree) = 
  case tree of
    Finished -> 
      -- Game has already finished, ignore
      return (n, Finished)
    Decision q lt rt -> do 
      -- Pose a question, use answer to traverse down
      ans <- question_prompt q
      if (ans == no)
      then do
        (newn,newlt) <- traverseWithCount (n,lt)
        if newlt == Finished 
        then return (newn, Finished)
        else return (newn, Decision q newlt rt)
      else do
        (newn,newrt) <- traverseWithCount (n,rt)
        if newrt == Finished 
        then return (newn, Finished)
        else return (newn, Decision q lt newrt)
    Leaf animal -> do 
      ans <- leaf_prompt animal
      if (ans == no)
      then do
        -- Gets more info from user, updates tree for next round
        new_data <- new_data_prompt animal
        start_round_prompt
        return (n+1, insert tree new_data)
      else do
        -- Game over - pass empty tree to kill future calls
        end_game_prompt n
        return (n, Finished)

{- Inserts a newly retrieved question/animal where program was wrong.
   :conflict: The animal that the program just guessed, but was wrong about.
-}
insert :: Tree -> InfoTuple -> Tree
insert conflict (newAnimal, q, newAns) = 
  Decision q left right
  where 
    (left, right) = 
      if (newAns == no)
      then (Leaf newAnimal, conflict)
      else (conflict, Leaf newAnimal) 

-- Prompts user for what they thought of to add to decision tree.
new_data_prompt :: String -> IO InfoTuple
new_data_prompt prior = do 
    hFlush stdout
    putStr $ "Sorry! What was your animal?\t"
    animal <- getLine
    putStr $ "Okay - now write a question to\ndiffer it from a " ++ prior ++ ":\t"
    question <- getLine
    putStr $ "Is this true for your animal?\t"
    answer <- getLine
    putStr $ "Thanks!\n\n|| NEXT ROUND ||\n\n"
    return (animal, question, answer)

-- Used at beginning of each round to prompt for thought.
start_round_prompt :: IO ()
start_round_prompt = do
  putStr "Think of an animal. Hit return when ready. "
  hFlush stdout
  getLine
  return ()

-- Game over message, displays total playthroughs required
end_game_prompt :: Int -> IO ()
end_game_prompt tries = 
  putStrLn $ "Haha! I guessed it with " ++ str_tries ++ "!"
  where 
    str_tries = 
      if tries == 1
      then "1 fail"
      else (show tries) ++ " fails"

-- Formats and asks a decision question to the user, retrieves answer
question_prompt :: String -> IO String
question_prompt q =
  do putStr $ "QUESTION > " ++ q ++ "\t"
     hFlush stdout
     getLine
 
-- Asks if user has thought of a specific animal, retrieves answer
leaf_prompt :: String -> IO String
leaf_prompt animal = 
  do putStr $ "QUESTION > Is it a " ++ animal ++ "?\t"
     hFlush stdout
     getLine
      
-- Example hierarchy generated after a game.
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