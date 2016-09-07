module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import qualified Data.Set as Set

----
-- GameState type. TODO is this a type?
----

data GameState = GameState {
    numCards :: Int, -- Might not be necessary.
    optionsSpace :: [[Card]]
} deriving (Show)


----
-- Main function prototypes.
----
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
initialGuess :: Int -> ([Card],GameState)
-- Takes guess and gamestate, and the feedback, and produces a new guess and gamestate.
nextGuess :: ([Card],GameState) ->(Int,Int,Int,Int,Int) -> ([Card],GameState)




----
-- Code for the feedback function.
----

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




----
-- Code for the initialGuess function.
----

-- Helper for getOptionsSpace. Ensures that all elements in a list are unique.
-- https://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique
allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs

-- Efficient implementation of nub.
nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

-- Enumerates all the possible guesses at the start of the game.
-- Works for any number of cards and watches out for duplicate and invalid guesses.
getOptionsSpace :: Int -> [[Card]]
getOptionsSpace i = nubOrd [sort x | x <- sequence $ replicate i cards, allDifferent x]
    where
    cards = [(Card Club R2) .. (Card Spade Ace)]

-- Takes every nth element of a list:
-- https://stackoverflow.com/questions/2026912/how-to-get-every-nth-element-of-an-infinite-list-in-haskell
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

-- Creates an optimal initial guess. This means each card has a different suit and the ranks
-- are equidistant from each other based on the number of cards in the guess (13/i).
initialGuess 0 = error "Need at least 1 card"
initialGuess i = (cards, state)
    where
    -- optionsSpace=[(Card Club R2) ..] should work, but doesn't for some reason.
    cards = zipWith Card suits ranks
        where
        suits = take i [Club ..]
        ranks = take i $ every (13 `div` i) [R2 ..]
    -- The space of options is all cards except for those already guessed.
    state = GameState {numCards=i, optionsSpace=(getOptionsSpace i) \\ [cards]}




----
-- Code for the nextGuess function.
----

-- Currently just keeps taking the next possible guess until we get it right.
nextGuess (prevGuess, state) feedback = (newGuess, newState)
    where
    newGuess = head $ optionsSpace state
    updateState s = s {optionsSpace = optionsSpace s \\ [newGuess]}
    newState = updateState state








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