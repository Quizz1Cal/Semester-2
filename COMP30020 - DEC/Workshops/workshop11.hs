-- Q1
-- Note the use of Integer
-- Consider less algorithmically complex solns
fibs :: Int -> [Integer]
fibs n = take n $ zipWith (+) (0:fibs n) (0:1:fibs n)

-- Q2
allfibs :: [Integer]
allfibs = zipWith (+) (0:allfibs) (0:1:allfibs)

-- Q3
-- LEARNT: To do STRICT, write the full data structure passed (list) and look at output 
-- after each op; for lists, count # cons
-- LEARNT: To do LAZY, look at what is suspended and max 'wait' possible

-- Q4
-- 

-- Q5
-- Also can't do it coz I don't know what an Mtree is!


