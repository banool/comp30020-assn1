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
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)




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
    _guessRanks  = sort [rank r | r <- guess]
    _answerRanks = sort [rank r | r <- answer]

    lower  = length $ filter (< head _guessRanks) _answerRanks
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

-- Creates an optimal initial guess. This means that each card has a different suit and the
-- ranks are equidistant from each other based on the number of cards in the guess (13/n).
-- Only need to consider up to 4 cards.
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

-- Returns a new options space without cards that can't be in the answer based on
-- removing cards than are lower than the lowest rank, vice versa for highest.
getSpaceNoLowerHigher :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> [[Card]]
getSpaceNoLowerHigher (prevGuess, state) fb = newSpace
    where
    _guessRanks  = sort [rank r | r <- prevGuess]
    _lowestRank  = head _guessRanks
    _highestRank = last _guessRanks

    _lower  = (\(_,a,_,_,_) -> a) fb
    _higher = (\(_,_,_,a,_) -> a) fb

    -- If no cards in the answer are lower rank than the lowest in the previous 
    -- guess, remove all guesses from the options space that have a rank lower 
    -- than the lowest in the previous guess.
    _noLower = 
        if _lower == 0
            -- Keep an option in the list if none of the cards are lower than the lowest.
            then [option | option <- optionsSpace state, not $ any (\c -> (rank c) < _lowestRank) [c | c <- option]]
            else optionsSpace state
    _noHigher = 
        if _higher == 0
            then [option | option <- _noLower, not $ any (\c -> (rank c) > _highestRank) [c | c <- option]]
            else optionsSpace state

    newSpace = _noHigher

-- Applies the algorithm for mastermind described here, except obviously for this game:
-- https://math.stackexchange.com/questions/1192961/knuths-mastermind-algorithm
-- It works like this:
--     Go through the possible guess space.
--     For each possibility, treat it if it were the answer.
--     Check if you get the same feedback with this as the answer compared to the previous guess.
--     If you don't get the same response, this possibility cannot be the answer.
getKnuthSpace :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> [[Card]]
getKnuthSpace (prevGuess, state) fb = newSpace
    where
    newSpace = [option | option <- optionsSpace state, feedback option prevGuess == fb]

-- Takes a gamestate and returns it with the new optionsSpace applied.
updateStateSpace :: GameState -> [[Card]] -> GameState
updateStateSpace gs newSpace = gs {optionsSpace = newSpace}


-- Currently just keeps taking the next possible guess until we get it right.
nextGuess (prevGuess, state) fb = (newGuess, newState)
    where
    -- 2/3. Get ranks lower than lowest in guess and ranks higher than highest in guess.
    _intermediateSpace1 = optionsSpace state \\ [prevGuess]
    _intermediateSpace2 = getSpaceNoLowerHigher (prevGuess, updatedSpace) fb
        where updatedSpace = updateStateSpace state _intermediateSpace1 
    _intermediateSpace3 = getKnuthSpace (prevGuess, updatedSpace) fb
        where updatedSpace = updateStateSpace state _intermediateSpace2

    _newSpace = _intermediateSpace3

    newGuess = head $ _newSpace
    newState = updateStateSpace state _newSpace


-- make guess





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