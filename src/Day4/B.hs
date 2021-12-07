module Day4.B where

import Day4.Input (draws, bingos)
import Day4.A (finishBingos)
import Data.List
import Data.Ord

-- Incorrect answer. Don't know why.
answer :: IO Int
answer = snd <$> (loser <$> draws <*> bingos)

loser :: [Int] -> [[[Int]]] -> (Int, Int)
loser draws = minimumBy (comparing fst) . finishBingos draws
