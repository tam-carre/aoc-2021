module Day1.A where

import Day1.Input (input)

answer :: Int
answer = countIncreases input

countIncreases :: [Int] -> Int
countIncreases = countIncreases' 0
  where countIncreases' total [] = total
        countIncreases' total [x] = total
        countIncreases' total (x:y:xs) =
          let addition = if y - x > 0 then 1 else 0
          in countIncreases' (total + addition) (y:xs)

