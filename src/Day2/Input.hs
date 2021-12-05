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
toCommand str =
  let cmdWords = words str
  in case (head cmdWords, read $ cmdWords !! 1) of
       ("forward", n) -> Fwd n
       ("down", n)    -> Down n
       ("up", n)      -> Up n
       (_, _)         -> Up 0
