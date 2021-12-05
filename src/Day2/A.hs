module Day2.A where

import Day2.Input

answer :: Int
answer = finalForwardness input * finalDepth input

finalForwardness :: [Command] -> Int
finalForwardness = foldl f 0
  where f forwardness (Forward n) = forwardness + n
        f forwardness _ = forwardness

finalDepth :: [Command] -> Int
finalDepth = foldl f 0
  where f depth (Down n) = depth + n
        f depth (Up n) = depth - n
        f depth _ = depth
