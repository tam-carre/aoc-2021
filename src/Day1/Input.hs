module Day1.Input where

input :: IO [Int]
input = map read . lines <$> readFile "./src/Day1/input.txt"
