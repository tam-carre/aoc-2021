module Day2.B where

import Day2.Input (input, Command (Forward, Down, Up))
import Day2.A (forwardness)
import Utils.General (fst3)

answer :: Int
answer = forwardness input * depth input

aim :: [Command] -> Int
aim = foldl f 0
  where f aim (Down n) = aim + n
        f aim (Up n) = aim - n
        f aim _ = aim

depth :: [Command] -> Int
depth = fst3 . foldl f ( 0, 0, [] )
  where
    f ( depth, aim, cmds ) (Down n) = ( depth, aim + n, cmds ++ [Down n] )
    f ( depth, aim, cmds ) (Up n) = ( depth, aim - n, cmds ++ [Up n] )
    f ( depth, aim, cmds ) (Forward n) = ( depth + n * aim, aim, cmds ++ [Forward n] )
