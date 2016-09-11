-- File     : Proj1.hs
-- Author   : Daniel Porteous porteousd 696965
-- Purpose  : Guessing card game for Project 1

-- Note: Links to stackoverflow indicate initial inspiration for the code. 
-- The code that appears here was written by me and will likely look different.

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import qualified Data.Set as Set


----
-- GameState record and global constants.
----

data GameState = GameState {
    numCards :: Int, -- Might not be necessary.
    optionsSpace :: [[Card]]
} deriving (Show)

cardsPerSuit = 13


----
-- Main function prototypes.
----
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
initialGuess :: Int -> ([Card],GameState)
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)


----
-- Code for the feedback function.
----

-- Intersect without duplicates.
-- http://goo.gl/nfq7pe
intersectNoDuplicates xs ys = xs \\ (xs \\ ys)


-- Takes the answer and guess and returns feedback. All 5 parts of the feedback 
-- are implemented in this function body, none were complicated enough to 
-- require their own functions.
feedback answer guess = (correct, lower, sameRank, higher, sameSuit) where
    -- 1. Get all the correct guesses.
    correct = length $ intersect guess answer

    -- 2/3. Get ranks lower than lowest in guess and ranks higher than highest.
    _guessRanks  = sort [rank r | r <- guess]
    _answerRanks = sort [rank r | r <- answer]

    lower  = length $ filter (< head _guessRanks) _answerRanks
    higher = length $ filter (> last _guessRanks) _answerRanks

    -- 4. Get same ranks.
    sameRank = length $ intersectNoDuplicates _answerRanks _guessRanks

    -- 5. Get same suits.
    sameSuit = length $ intersectNoDuplicates [suit s | s <- answer] [suit s | s <- guess]


----
-- Code for the initialGuess function.
----

-- Helper for getOptionsSpace. Ensures that all elements in a list are unique.
-- http://goo.gl/IJJAs3
allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs


-- Efficient implementation of nub.
-- https://hackage.haskell.org/package/extra-1.5/docs/Data-List-Extra.html
-- This is just for time improvements, regular nub could just as easily be used.
nubOrd :: Ord a => [a] -> [a] 
nubOrd xs = go Set.empty xs
    where
    go s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise        = x : go (Set.insert x s) xs
    go _ _                 = []


-- Enumerates all the possible guesses at the start of the game.
-- Works for any number of cards and checks for duplicate and invalid guesses.
getOptionsSpace :: Int -> [[Card]]
getOptionsSpace i = nubOrd [sort x | x <- sequence $ replicate i cards, allDifferent x]
    where
    -- cards = [(Card Club R2) ..] should work, but doesn't for some reason.
    cards = [(Card Club R2) .. (Card Spade Ace)]


-- Takes every nth element of a list:
-- http://goo.gl/cSBnz5
-- The drop function returns the latter part of the list starting from n.
every n xs = case drop (n-1) xs of
            (y:ys)  -> y : every n ys
            []      -> []


-- Creates an optimal initial guess. This means that each card has a different 
-- suit and the ranks are equidistant from each other based on the number of 
-- cards in the guess (13/n). Only need to consider up to 4 cards.
initialGuess 0 = error "Need at least 1 card"
initialGuess i = (cards, state)
    where
    cards = zipWith Card suits ranks
        where
        suits = take i [Club ..]
        ranks = take i $ every (cardsPerSuit `div` i) [R2 ..]
    -- The space of options is all cards except for those already guessed.
    state = GameState {numCards=i, optionsSpace=(getOptionsSpace i) \\ [cards]}


----
-- Code for the nextGuess function.
----

-- Returns a new optionsSpace without cards that can't be in the answer based on
-- removing cards than are lower than the lowest rank, vice versa for highest.
-- This function reduces the space by far less than the getKnuthSpace function,
-- if at all, but is a demonstration of a more bulky function.
getSpaceNoLowerHigher :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> [[Card]]
getSpaceNoLowerHigher (prevGuess, state) fb = newSpace
    where
    _guessRanks  = sort [rank r | r <- prevGuess]
    _lowestRank  = head _guessRanks
    _highestRank = last _guessRanks

    _lower  = (\(_,a,_,_,_) -> a) fb
    _higher = (\(_,_,_,a,_) -> a) fb

    _prevSpace = optionsSpace state

    -- If no cards in the answer are lower rank than the lowest in the previous 
    -- guess, remove all guesses from the options space that have a rank lower 
    -- than the lowest in the previous guess.
    _noLower = 
        if _lower == 0
            -- Keep an option in the optionsSpace list only if none of the 
            -- cards are lower than the lowest.
            then [option | option <-  _prevSpace, 
                not $ any (\c -> (rank c) < _lowestRank) [c | c <- option]]
            else  _prevSpace
    _noHigher = 
        if _higher == 0
            then [option | option <- _noLower, 
                not $ any (\c -> (rank c) > _highestRank) [c | c <- option]]
            else _noLower

    newSpace = _noHigher


-- Applies the algorithm for mastermind option space reduction described here:
-- http://goo.gl/J8x2Wk
-- It works like this:
--     Go through the possible guess space.
--     For each possibility, treat it if it were the answer.
--     Check if you get the same feedback with this as the answer compared to 
--     the previous guess. If you don't get the same response, this 
--     possibility cannot be the answer.
getKnuthSpace :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> [[Card]]
getKnuthSpace (prevGuess, state) fb = newSpace
    where
    newSpace = [option | option <- optionsSpace state, feedback option prevGuess == fb]


-- Takes a gamestate and optionsSpace and returns it with the new optionsSpace.
updateStateSpace :: GameState -> [[Card]] -> GameState
updateStateSpace gs newSpace = gs {optionsSpace = newSpace}


-- This function applies a series of steps to the space before deciding upon
-- a new guess. It would be nice to do point free style, but the return types
-- of the above functions make it difficult to implement elegantly.
-- As such, the function creates intermediate spaces which track changes
-- throughout moving from the previous state to the new state. 
-- This is good from a diagnostics standpoint because its easy to check 
-- what changes with each operation.
nextGuess (prevGuess, state) fb = (newGuess, newState)
    where
    
    -- Remove the previous guess from the state since it has been checked.
    _intermediateSpace1 = optionsSpace state \\ [prevGuess]

    -- Get ranks lower than lowest in guess and ranks higher than highest.
    _intermediateSpace2 = getSpaceNoLowerHigher (prevGuess, updatedSpace) fb
        where updatedSpace = updateStateSpace state _intermediateSpace1

    -- Apply the Knuth algorithm for optionsSpace reduction.
    _intermediateSpace3 = getKnuthSpace (prevGuess, updatedSpace) fb
        where updatedSpace = updateStateSpace state _intermediateSpace2

    _newSpace = _intermediateSpace3

    newGuess = head _newSpace
    newState = updateStateSpace state _newSpace
