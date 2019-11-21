{-
File     : Proj1.hs
Author   : Callum Holmes
Purpose  : Implements Feedback & answerer AI for guessing game

This is my submission for Project 1, implementing feedback and answerer.
The answerer attempts to maximise the entropy of a split on the possible
answers remaining after each guess, and chooses the guess that it estimates to
be the best. This implementation determines independently the best ranks, and
suits, that split the cards, before calculating from these the best cards; 
though this introduces code complexity, it reduces an O(N^2) search to O(R^2)
where R is the number of all possible rank combinations (size of rankspace).
-}

module Proj1 (feedback, nextGuess, initialGuess, GameState) where

import Card
import Data.List (permutations, sortBy, nub, sort, (\\), intersect)
import Data.Char (intToDigit)
import Data.Map (Map, insertWith, toList, empty)

-- | Types and Globals

-- GameStates consist of four components:
--   rankSpace              : all possible rank sets for the answer
--   suitSpace              : all possible suit sets for the answer
--   priors (priorGuesses)  : most recent is first
--   priorCorrects          : the # correct CARDS for each priorGuess
type GameState = ([[Rank]], [[Suit]], [[Card]], [Int])
-- Output of a partitioning algorithm; maps each item to a dummy string.
type Partition a = Map String [a]
-- Denotes functions that categorise partitionable data.
type Tagger a = Int -> [a] -> [a] -> String

-- Range of ranks between R2 and RA
rankRange = 13 :: Int

-- | Guess Logic

-- Given dimension n <= 12, returns initial guess / GameState
initialGuess :: Int -> ([Card], GameState)
initialGuess n =
    (guess, (rankSpace, suitSpace, [], []))
    where 
        rankSpace = createSpace [minBound..maxBound] n :: [[Rank]]
        suitSpace = createSpace [minBound..maxBound] n :: [[Suit]]

        -- Choose ranks that split the rank-range uniformly
        gap = rankRange `quot` (n+1)
        addGap = iterate (succ .) id !! gap
        ranks = tail $ iterate addGap R2

        -- Choose suits for guess, compute guess
        suits = cycle [Spade, Heart]
        guess = take n $ zipWith Card suits ranks

-- Returns most optimal guess given feedback and gamestate
nextGuess :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> ([Card], GameState)
nextGuess (guess, (ranks, suits, priors, priorCorrects)) fback = 
    let guessRank = map rank guess
        guessSuit = map suit guess
        n = length guess
        (corrCards, lowRank, corrRank, highRank, corrSuit) = fback

        -- Filter for proper suits and ranks with feedback data
        priors' = guess : priors
        priorCorrects' = corrCards : priorCorrects
        suits' = filter (\x -> corrSuit == corrSuits x guessSuit) suits
        ranks' = filter consistentRanks ranks
        consistentRanks ansRank = 
            lowRank == lowRanks ansRank guessRank
            && corrRank == corrRanks ansRank guessRank
            && highRank == highRanks ansRank guessRank
        state' = (ranks', suits', priors', priorCorrects')
    in (bestGuess n state', state')

-- Chooses best guess with max entropy in rankSpace and suitSpace
bestGuess :: Int -> GameState -> [Card]
bestGuess n (ranks, suits, priors, priorCorrects) = 
    let rankEnts = map 
          (\x -> entropy $ (makePartition rankTagger) n x ranks) ranks
        bestRanks = map snd . sortBy comparator $ zip rankEnts ranks
        suitEnts = map 
          (\x -> entropy $ (makePartition suitTagger) n x suits) suits
        bestSuits = map snd . sortBy comparator $ zip suitEnts suits
        comparator x y = compare (fst y) (fst x)
    in head $ generateCards (bestRanks, bestSuits, priors, priorCorrects)

-- Calculates entropy for any non-empty partition. 
entropy :: Partition a -> Double
entropy part = 
    let sizes = map (fromIntegral . length . snd) $ toList part
        total = sum sizes
        proportions = map (/ total) sizes
    in sum $ map (\p -> (-p) * log p) proportions 

-- | Field-space partitioning

-- Partition a domain using appropriate categoriser, given guess
makePartition :: Eq a => Tagger a -> Int -> [a] -> [[a]] -> Partition [a]
makePartition _ _ _ [] = empty
makePartition tagger n guess (ans:ps) = 
    let index = tagger n ans guess
        remainder = makePartition tagger n guess ps
    in  -- Exclude guess from the actual partition
        if guess == ans
        then remainder
        else insertWith (++) index [ans] remainder

-- Creates a string tag indicating number of correct suits.
suitTagger :: Tagger Suit
suitTagger n ans guess =
    let corrSuit = n - length (ans \\ guess)
    in intToDigit corrSuit : []
    
-- Creates a string tag indicating the rank bounds.
-- As ranks can be 0 to N less than the left AND/OR right 'bound'
-- from lowRanks/highRanks, represent the 6 groups as 00,0N,N0,0K,K0,KK.
-- (Note: KN, NK, NN mathematically impossible by construction)
rankTagger :: Tagger Rank
rankTagger n ans guess = 
    let index val = 
            if (val `elem` [0,n]) then intToDigit val else 'K'
    in map index [lowRanks ans guess, highRanks ans guess]

-- | Card/space generation

-- Given a list of rank/suit groups and priors, generates ans answers
-- Note that it generates in-order, with rank order as priority
generateCards :: GameState -> [[Card]]
generateCards (ranks, suits, priorGuesses, priorCorrects) = 
    concat [combinations rankSet suitSet | rankSet <- ranks, suitSet <- suits]
    where
        -- Given a rank/suit guesses, generate all valid unique combinations
        combinations rankSet suitSet = 
            filter consistentCards . nub $ [
                sort $ zipWith Card suits ranks |
                ranks <- permutations rankSet, 
                suits <- permutations suitSet
                ]

        -- Filter possible answers with correct # cards for ALL priors
        consistentCards ans = 
            (nub ans == ans) 
                && (and . map (\(guess, corrC) -> corrC == corrCards ans guess)
                $ zip priorGuesses priorCorrects)  

-- Generates all unique subsets of order n recursively for a given domain
createSpace :: Ord a => [a] -> Int -> [[a]]
createSpace source n = 
    nub . map sort . foldr ($) [] $ replicate n nextSpace
    where
        -- Given priorSpace of subsets size (n-1), computes nth space
        nextSpace [] = map (:[]) source
        nextSpace priorSpace = 
            [new:priors | new <- source, priors <- priorSpace]

-- | Feedback calculations

-- Takes a target and guess, returning feedback indicators
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback ans guess = 
    (corrCards ans guess, 
        lowRanks ansRank guessRank, 
        corrRanks ansRank guessRank, 
        highRanks ansRank guessRank, 
        corrSuits ansSuit guessSuit)
    where 
        ansRank = map rank ans
        guessRank = map rank guess
        ansSuit = map suit ans
        guessSuit = map suit guess
  
-- Number of correct cards b/t two guesses
corrCards :: [Card] -> [Card] -> Int
corrCards ans guess = length $ intersect ans guess

-- Number of answer's ranks the guess' minimum rank exceeds
lowRanks :: [Rank] -> [Rank] -> Int
lowRanks ansRank guessRank = 
    length $ filter (< minimum guessRank) ansRank

-- Number of correct ranks b/t two guesses
corrRanks :: [Rank] -> [Rank] -> Int
corrRanks ansRank guessRank = 
    length ansRank - length (ansRank \\ guessRank)

-- Number of answer's ranks the guess' maximum rank precedes
highRanks :: [Rank] -> [Rank] -> Int
highRanks ansRank guessRank = 
    length $ filter (> maximum guessRank) ansRank

-- Number of correct suits b/t two guesses
corrSuits :: [Suit] -> [Suit] -> Int
corrSuits ansSuit guessSuit = 
    length ansSuit - length (ansSuit \\ guessSuit)