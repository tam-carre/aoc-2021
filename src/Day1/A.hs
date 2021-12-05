module Day1.A where

import Day1.Input (input)

answer :: Int
answer = countIncreases input

countIncreases :: [Int] -> Int
countIncreases []     = 0
countIncreases (x:xs) = fst . foldl f (0, x) $ xs
  where f (incrs, prev) x = if x - prev > 0 then (incrs + 1, x)
                                            else (incrs, x)
