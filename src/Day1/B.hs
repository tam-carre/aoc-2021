module Day1.B where

import Day1.Input (input)
import Day1.A (countIncreases)

answer :: IO Int
answer = countIncreases . toThreeItemSums <$> input

toThreeItemSums :: [Int] -> [Int]
toThreeItemSums []         = []
toThreeItemSums [_]        = []
toThreeItemSums [_, _]     = []
toThreeItemSums [a, b, c]  = [sum [a, b, c]]
toThreeItemSums (a:b:c:xs) = sum [a, b, c] : toThreeItemSums (b:c:xs)
