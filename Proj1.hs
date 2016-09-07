module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

data GameState = GameState {
    numGuesses :: Int,
    numCards :: Int,
    optionsSpace :: [Card],
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

--initalCards :: Int -> Int -> GameState -> [Card]
--initialCards start inc s = 

getCards :: Int -> GameState -> [Card]
getCards 1 s = [head $ optionsSpace s]
getCards i s = (head $ optionsSpace s) : getCards (i-1) ns
    where
    updateState s = s {optionsSpace = tail $ optionsSpace s}
    ns = updateState s

-- selectCard :: GameState -> Card

initialGuess 0 = error "Need at least 1 card"
initialGuess i = (cards, state)
    where
    -- optionsSpace=[(Card Club R2) ..] should work, but doesn't for some reason.
    cards = zipWith Card suits ranks
        where
        suits = take i [Club ..]
        ranks = take i $ every (13 `div` i) [R2 ..]
    -- The space of options is all cards except for those already guessed.
    state = GameState {numGuesses=0, numCards=i, optionsSpace=[(Card Club R2) .. (Card Spade Ace)] \\ cards, won=False}

nextGuess (prevGuess, state) feedback = (newGuess, newState)
    where
    newGuess = getCards (numCards state) state
    updateState s = s {optionsSpace = optionsSpace s \\ newGuess}
    newState = updateState state