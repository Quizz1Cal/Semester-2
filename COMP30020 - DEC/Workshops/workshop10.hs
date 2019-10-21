-- Q1

import qualified Data.Char as Char

maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail xs = Just $ tail xs -- maybe_tail (_:xs) = Just xs

ez_maybe_drop :: Int -> [a] -> Maybe [a]
ez_maybe_drop _ [] = Nothing
ez_maybe_drop n (x:xs)
    | n <= 0             = Just (x:xs)
    | n <= length (x:xs) = ez_maybe_drop (n-1) xs
    | otherwise          = Nothing

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
maybe_drop :: Int -> [a] -> Maybe [a]
maybe_drop _ [] = Nothing
maybe_drop 0 xs = Just xs
maybe_drop n (x:xs) = 
    -- Use maybe_tail (x:xs) as arg to maybe_drop (n-1), allowing for nondet
   maybe_tail (x:xs) >>= maybe_drop (n-1)

data Tree a = Empty | Node (Tree a) a (Tree a) 

print_tree :: Show a => Tree a -> IO ()
print_tree Empty = return ()
print_tree (Node l v r) =
  print_tree l 
  -- >> putStrLn (show v)
    >>= \_ -> putStrLn (show v) 
    -- >> print_tree r
      >>= \_ -> print_tree r

{- With do notation
print_tree :: Show a => Tree a -> IO () 
print_tree Empty = return ()
print_tree (Node l x r) = do
    print_tree l
    putStrLn $ show x
    print_tree r
-}

-- Q3
str_to_num :: String -> Maybe Int
str_to_num [] = Nothing
str_to_num [x] = 
    if Char.isDigit x
    then Just $ Char.digitToInt x
    else Nothing
str_to_num (x:xs) = 
  do
    n <- str_to_num xs
    d <- str_to_num [x]
    Just $ d*10^(length xs) + n

-- Q4
sum_reading :: Int
sum_reading =
  do
    x <- getLine
    if Char.isDigit (read x :: Int)
    then do
      val <- read x
      val + sum_reading
    else read x

-- Q5

execer :: 
read_command :: IO Char
read_command =
  do
    x <- getLine
    x !! 0


{-
-- Q3
str_to_num :: String -> Maybe Int
str_to_num [] = Nothing
str_to_num xs
    | Char.isDigit x = 
        if (length left > 0) 
        then do
            prev <- str_to_num left
            return $ 10*prev + v
        else Just v
    | otherwise = Nothing
    where x = last xs
          v = Char.digitToInt $ x
          left = init xs

-- Q4

-- Q5
-- Why so cruel
-}