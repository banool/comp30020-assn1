import Card

data GameState = GameState {
    numGuesses :: Int,
    optionsSpace :: [Card],
    won :: Bool -- True if won, false otherwise.
} deriving (Show)