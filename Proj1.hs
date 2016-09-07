module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import qualified Data.Set as Set

data GameState = GameState {
    numGuesses :: Int,
    numCards :: Int,
    optionsSpace :: [[Card]],
    won :: Bool -- True if won, false otherwise.
} deriving (Show)

-- Answer -> Guess -> Feedback
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
initialGuess :: Int -> ([Card],GameState)
-- Takes guess and gamestate, and the feedback, and produces a new guess and gamestate.
nextGuess :: ([Card],GameState) ->(Int,Int,Int,Int,Int) -> ([Card],GameState)

-- Intersect without duplicates.
-- https://stackoverflow.com/questions/27332815/haskell-intersection-with-duplicates
intersectNoDuplicates xs ys = xs \\ (xs \\ ys)

feedback answer guess = (correct, lower, sameRank, higher, sameSuit) where
    -- 1. Get all the correct guesses.
    correct = length $ intersect guess answer

    -- 2/3. Get ranks lower than lowest in guess and ranks higher than highest in guess.
    _sortedGuess    = sort guess
    _guessRanks     = [rank r | r <- _sortedGuess]
    _answerRanks    = [rank r | r <- answer]

    lower = length $ filter (< head _guessRanks) _answerRanks
    higher = length $ filter (> last _guessRanks) _answerRanks

    -- 4. Get same ranks.
    sameRank = length $ intersectNoDuplicates _answerRanks _guessRanks -- todo verify that these actually work.

    -- 5. Get same suits.
    sameSuit = length $ intersectNoDuplicates [suit s | s <- answer] [suit s | s <- guess] -- todo verify that these actually work.

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []


initialGuess 0 = error "Need at least 1 card"
initialGuess i = (cards, state)
    where
    -- optionsSpace=[(Card Club R2) ..] should work, but doesn't for some reason.
    cards = zipWith Card suits ranks
        where
        suits = take i [Club ..]
        ranks = take i $ every (13 `div` i) [R2 ..]
    -- The space of options is all cards except for those already guessed.
    state = GameState {numGuesses=0, numCards=i, optionsSpace=(getOptionsSpace i) \\ [cards], won=False}

-- One way of maybe getting all possible guesses:
-- [x | x <- subsequences [(Card Club R2) .. (Card Spade Ace)], length x == 2]
-- Another (much better) way:
-- cartProd xs ys = [[x,y] | x <- xs, y <- ys]
-- cartProd [(Card Club R2) .. (Card Spade Ace)] [(Card Club R2) .. (Card Spade Ace)]
-- The above however allows invalid guesses like (2C, 2C).
-- The best way to do this is:
-- cartProd xs ys = [[x,y] | x <- xs, y <- ys, x /= y]
-- We still have a problem, duplicate guesses, e.g. (2C, 3C) and (3C, 2C).
-- This code gets around that also:
-- cartProd xs ys = nub [(sort [x,y]) | x <- xs, y <- ys, x /= y]
-- Finally call it with this:
-- cartProd [(Card Club R2) .. (Card Spade Ace)] [(Card Club R2) .. (Card Spade Ace)]
-- This only currently works for guesses of length 2.

allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs

nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

getOptionsSpace :: Int -> [[Card]]
getOptionsSpace i = nubOrd [sort x | x <- sequence $ replicate i cards, allDifferent x]
    where
    cards = [(Card Club R2) .. (Card Spade Ace)]



nextGuess (prevGuess, state) feedback = (newGuess, newState)
    where
    newGuess = head $ optionsSpace state
    updateState s = s {optionsSpace = optionsSpace s \\ [newGuess]}
    newState = updateState state