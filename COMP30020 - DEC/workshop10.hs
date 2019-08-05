-- Q1

import qualified Data.Char as Char

maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail xs = Just $ tail xs

ez_maybe_drop :: Int -> [a] -> Maybe [a]
ez_maybe_drop _ [] = Nothing
ez_maybe_drop n (x:xs)
    | n <= 0             = Just (x:xs)
    | n <= length (x:xs) = ez_maybe_drop (n-1) xs
    | otherwise          = Nothing

-- Q2

data Tree a = Empty | Node (Tree a) a (Tree a) 

print_tree :: Show a => Tree a -> IO () 
print_tree Empty = return ()
print_tree (Node l x r) = do
    print_tree l
    putStrLn $ show x
    print_tree r

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
-- hmmm...

-- Q5
-- Why so cruel


