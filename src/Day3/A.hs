module Day3.A where

import Day3.Input (input)
import Data.Char (digitToInt)
import Data.List
import Data.Ord

answer :: Int
answer = gamma input * epsilon input

gamma :: [String] -> Int
gamma = binaryStringToDec . gamma'

gamma' :: [String] -> String
gamma' = map mostCommon . columns

epsilon :: [String] -> Int
epsilon = binaryStringToDec . epsilon'

epsilon':: [String] -> String
epsilon' = map reverseBit . gamma'

reverseBit :: Char -> Char
reverseBit bit = if bit == '0' then '1' else '0'
  
binaryStringToDec :: String -> Int
binaryStringToDec = foldl f 0
  where f dec bit = dec * 2 + digitToInt bit

columns :: [[a]] -> [[a]]
columns = transpose

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort
