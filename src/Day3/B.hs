module Day3.B where

import Day3.Input (input)
import Day3.A (binaryStringToDec, gamma', epsilon')

answer :: IO Int
answer = do
  oxygenRating <- oxygenGen <$> input
  co2Rating    <- co2Scrub <$> input
  return $ oxygenRating * co2Rating

oxygenGen :: [String] -> Int
oxygenGen = binaryStringToDec . foldByCriteria gamma'

co2Scrub :: [String] -> Int
co2Scrub = binaryStringToDec . foldByCriteria epsilon'

foldByCriteria :: ([String] -> String) -> [String] -> String
foldByCriteria criteria = foldByCriteria' 0
  where foldByCriteria' pos nums =
          let filtered = filterByCriteria criteria pos nums
          in if length filtered == 1
               then head filtered
               else foldByCriteria' (pos + 1) filtered

filterByCriteria :: ([String] -> String) -> Int -> [String] -> [String]
filterByCriteria criteria pos nums = filter f nums
  where f num = (criteria nums !! pos) == (num !! pos)
