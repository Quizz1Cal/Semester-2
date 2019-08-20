-- Q1
-- Note the use of Integer
-- Consider less algorithmically complex solns
fibs :: Int -> [Integer]
fibs n = take n $ zipWith (+) (0:fibs n) (0:1:fibs n)

-- Q2
allfibs :: [Integer]
allfibs = zipWith (+) (0:allfibs) (0:1:allfibs)

-- Q3, Q4
-- Can't do it yet as haven't really learnt how it handles strict/lazy

-- Q5
-- Also can't do it coz I don't know what an Mtree is!


