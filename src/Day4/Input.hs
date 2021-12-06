module Day4.Input (draws, bingos) where

import Data.List.Split

draws :: IO [Int]
draws = fst <$> input

bingos :: IO [[[Int]]]
bingos = snd <$> input

input :: IO ([Int], [[[Int]]])
input = do
  everything <- lines <$> readFile "./src/Day4/input.txt"
  let draws = map read . splitOn "," . head $ everything
  let bingos = linesToInts . splitAtEmptyLine . tail . tail $ everything
  return (draws, bingos)

splitAtEmptyLine :: [String] -> [[String]]
splitAtEmptyLine = foldl f [[]]
  where f lists "" = lists ++ [[]]
        f lists line = init lists ++ [last lists ++ [line]]

linesToInts :: [[String]] -> [[[Int]]]
linesToInts = map . map $ map read . words
