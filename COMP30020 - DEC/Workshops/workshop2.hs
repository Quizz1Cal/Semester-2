-- Q1 ... really?

{- To implement a 52-card deck:
- 1. List of integers for ieach
- 2. List of structs, using int/char
- 3. Set
- 4. Binary tree/heap of these structs
- 5. Char representation for each, listed

ENUMERATION
Pairs of ENUMS --> No mapping restr though...
Pairs of STRINGS
Number for EACH --> Encode the suits in # 13's
JPEG
URI
-}

-- Q2
data Colour = Name String | Hex Int | RGB Int Int Int
data Tag = Size Int | Face String | Colour Colour
type FontTag = [Tag]
-- They chose not to use tupling for the RGB
-- They had the same idea for the fonttags though.
-- However as multiple tags of same type is bad, they opted for
data FontTagBest = Font_tag (Maybe Int) (Maybe String) (Maybe Font_color)

-- Q3
factorial :: Integer -> Integer
factorial n
  | n > 1 = n * factorial(n-1)
  | otherwise = 1

-- Q4
myElem :: Eq a => [a] -> a -> Bool
myElem [] _ = False
myElem (x:xs) key 
  | x == key = True
  | otherwise = myElem xs key

-- Q5
-- Severe design law: prefixes MUST start from front, so no skipping.
longestPrefix :: Eq a => [a] -> [a] -> [a]
longestPrefix (x:xs) (y:ys)
  | x == y = [x] ++ longestPrefix xs ys
  | otherwise = []
longestPrefix _ _ = []

-- Q6
mccarthy_91 :: Integer -> Integer
mccarthy_91 n = mcc n 1 
  where 
    mcc n c
      | c == 0 = n
      | otherwise =
        if n > 100
        then mcc (n-10) (c-1)
        else mcc (n+11) (c+1)

-- Q7
harder :: Integer -> Integer -> [Integer]
harder min max 
  | min <= max = min : harder (min+1) max
  | otherwise = []