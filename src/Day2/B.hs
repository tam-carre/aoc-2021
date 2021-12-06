module Day2.B where

import Day2.Input (input, Command (Fwd, Down, Up))
import Day2.A (fwdness)
import Data.Functor

answer :: IO Int
answer = do
  cmds <- input
  return $ fwdness cmds * depth cmds

depth :: [Command] -> Int
depth = fst . foldl f (0, 0)
  where f (depth, aim) (Down n) = (depth, aim + n)
        f (depth, aim) (Up n)   = (depth, aim - n)
        f (depth, aim) (Fwd n)  = (depth + n * aim, aim)
