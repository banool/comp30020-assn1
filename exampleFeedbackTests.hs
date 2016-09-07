import Proj1
import Card
import Data.List

-- Answer first, guess second.
answer1 = [(Card Club R3), (Card Heart R4)]
guess1  = [(Card Heart R4), (Card Club R3)]

answer2 = [(Card Club R3), (Card Heart R4)]
guess2  = [(Card Club R3), (Card Heart R3)]

answer3 = [(Card Diamond R3), (Card Heart R3)]
guess3  = [(Card Spade R3), (Card Club R3)]

answer4 = [(Card Club R3), (Card Heart R4)]
guess4  = [(Card Heart R2), (Card Heart R3)]

answer5 = [(Card Club Ace), (Card Club R2)]
guess5  = [(Card Club R3), (Card Heart R4)]

main = do
    print $ "Q1: " ++ (show $ feedback answer1 guess1) ++ " = 2,0,2,0,2"
    print $ "Q2: " ++ (show $ feedback answer2 guess2) ++ " = 1,0,1,1,2"
    print $ "Q3: " ++ (show $ feedback answer3 guess3) ++ " = 0,0,2,0,0"
    print $ "Q4: " ++ (show $ feedback answer4 guess4) ++ " = 0,0,1,1,1"
    print $ "Q5: " ++ (show $ feedback answer5 guess5) ++ " = 0,1,0,1,1"
