{-# LANGUAGE ViewPatterns #-}

module Day2.Input
  ( input
  , Command (..)
  ) where

data Command
  = Fwd Int
  | Down Int
  | Up Int

input :: IO [Command]
input = map toCommand . lines <$> readFile "./src/Day2/input.txt"

toCommand :: String -> Command
toCommand (words -> [verb, n]) = case (verb, read n) of
  ("forward", n) -> Fwd n
  ("down", n)    -> Down n
  ("up", n)      -> Up n
