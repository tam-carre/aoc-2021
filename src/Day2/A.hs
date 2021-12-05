module Day2.A where

import Day2.Input (input, Command (Forward, Down, Up))

answer :: Int
answer = forwardness input * depth input

forwardness :: [Command] -> Int
forwardness = foldl f 0
  where f forwardness (Forward n) = forwardness + n
        f forwardness _ = forwardness

depth :: [Command] -> Int
depth = foldl f 0
  where f depth (Down n) = depth + n
        f depth (Up n) = depth - n
        f depth _ = depth
