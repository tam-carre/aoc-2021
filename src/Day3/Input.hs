module Day3.Input where

input :: IO [String]
input = lines <$> readFile "./src/Day3/input.txt"
