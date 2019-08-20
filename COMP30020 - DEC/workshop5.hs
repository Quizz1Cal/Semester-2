-- Workshop 5 BOI

-- Q1
-- SAME AS FMAP REMEMBER AS MAYBES ARE FUNCTORS
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f (Just x) = Just $ f x
maybeApply f Nothing = Nothing

-- Q2: coding zipWith. 
-- NOTE THAT ZIP TRUNCATES TO SHORTEST
zWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zWith f as bs
  = [f a b | (a,b) <- zip as bs]

-- Q3. PLEASE
linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn a b xs = map ((+b) <$> (*a)) xs

-- Q4. PLEASE
sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
 | x > 0 = let y = sqrt x in [y, -y]
 | x == 0 = [0]
 | otherwise = [] 

allSqrts :: (Floating a, Ord a) => [a] -> [a]
allSqrts xs = foldl (++) [] $ map sqrtPM xs

-- Q5. 
-- Part A: simple higher-order mastery
sqrtPos :: (Floating a, Ord a) => [a] -> [a]
sqrtPos xs = map sqrt . filter (>=0) $ xs

-- Part B: do the above without higher-order
sqrtPosSad :: (Floating a, Ord a) => [a] -> [a]
sqrtPosSad (x:xs) = 
  if (x>=0)
  then [sqrt x] ++ sqrtPosSad xs
  else sqrtPosSad xs
sqrtPosSad _ = []

-- Part C: Make A one-sweep... NO SENSE
