{-# LANGUAGE TupleSections #-}

module Day4.A where

import Day4.Input (draws, bingos)
import Data.List
import Data.Ord

answer :: IO Int
answer = do
  bingos <- map toCrossable <$> bingos
  draws' <- draws
  let completedBingos = map (finishBingo draws') bingos
      winner          = minimumBy (comparing fst) completedBingos
  return $ snd winner

finishBingo :: [Int] -> [[(Int, Bool)]] -> (Int, Int)
finishBingo draws bingo = toTurnsAndScore . foldl f (0, bingo, 0) $ draws
  where f (totalDraws, bingoState, lastDraw) draw =
          if finished bingoState
            then (totalDraws, bingoState, lastDraw)
            else (totalDraws + 1, addDraw draw bingoState, draw)
        toTurnsAndScore (totalDraws, bingoState, lastDraw) =
          (totalDraws, score lastDraw bingoState)

score :: Int -> [[(Int, Bool)]] -> Int
score lastDraw bingo = unmarkedSum * lastDraw
  where unmarkedSum = sum . map fst . filter (not . snd) . concat $ bingo

finished :: [[(Int, Bool)]] -> Bool
finished bingo = anyRowFinished || anyColFinished
  where anyRowFinished = any (all snd) bingo
        anyColFinished = any (all snd) bingo

addDraw :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
addDraw draw = map $ map f
  where f cell = if fst cell == draw then (fst cell, True) else cell

toCrossable :: [[Int]] -> [[(Int, Bool)]]
toCrossable = map $ map (, False)
