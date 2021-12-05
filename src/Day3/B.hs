module Day3.B where

import Day3.Input (input)
import Day3.A (binaryStringToDec, gamma', epsilon')

answer :: Int
answer = oxygenGen input * co2Scrub input

oxygenGen :: [String] -> Int
oxygenGen = binaryStringToDec . foldByCriteria gamma'

co2Scrub :: [String] -> Int
co2Scrub = binaryStringToDec . foldByCriteria epsilon'

foldByCriteria :: ([String] -> String) -> [String] -> String
foldByCriteria criteria = foldByCriteria' 0
  where
    foldByCriteria' pos nums =
      if length filtered == 1
        then head filtered
        else foldByCriteria' (pos + 1) filtered
        where filtered = filterByCriteria criteria pos nums

filterByCriteria :: ([String] -> String) -> Int -> [String] -> [String]
filterByCriteria criteria pos nums = filter f nums
  where f num = (criteria nums !! pos) == (num !! pos)
