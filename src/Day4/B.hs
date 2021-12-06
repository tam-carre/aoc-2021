module Day4.B where

import Day4.Input (draws, bingos)
import Day4.A (finishBingos)
import Data.List
import Data.Ord

answer :: IO Int
answer = snd . maximumBy (comparing fst) <$> (finishBingos <$> draws <*> bingos)
