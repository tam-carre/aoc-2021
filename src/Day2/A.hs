module Day2.A where

import Day2.Input (input, Command (Fwd, Down, Up))

answer :: IO Int
answer = do
  cmds <- input
  return $ fwdness cmds * depth cmds

fwdness :: [Command] -> Int
fwdness = foldl f 0
  where f fwdness (Fwd n) = fwdness + n
        f fwdness _       = fwdness

depth :: [Command] -> Int
depth = foldl f 0
  where f depth (Down n) = depth + n
        f depth (Up n)   = depth - n
        f depth _        = depth
