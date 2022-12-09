{-# LANGUAGE BangPatterns #-}

module Day15 where

import Data.IntMap.Strict (IntMap, alter, empty, insert, (!))

data Log a = Log {latest :: a, secondLatest :: Maybe a}
  deriving (Show)

newLog :: a -> Log a
newLog a = Log a Nothing

diff :: Log Int -> Int
diff l = maybe 0 (latest l -) (secondLatest l)

update :: a -> Log a -> Log a
update a l = l {latest = a, secondLatest = Just (latest l)}

type LogMap = IntMap (Log Int)

updateMap :: Int -> Int -> LogMap -> LogMap
updateMap pos = alter alterFunction
  where
    alterFunction :: Maybe (Log Int) -> Maybe (Log Int)
    alterFunction = Just . maybe (newLog pos) (update pos)

next :: LogMap -> Int -> Int -> (Int, LogMap)
next lm pos lst = (i, newLm)
  where
    currentLog = lm ! lst
    i = diff currentLog
    newLm = updateMap pos i lm

-- This solution is shockingly slow for the second solution, about 2.5 minutes in GHCi, about a minute compiled
fromInitial :: Int -> [Int] -> Int
fromInitial stopAt is = step stopAt (1 + length is, last is, initialLogMap)
  where
    step s (!pos, !lst, !lm)
      | s == 0 = lst
      | otherwise = step (s - 1) (go (pos, lst, lm))

    go (pos, lst, lm) = (1 + pos, nLst, nLm)
      where
        (nLst, nLm) = next lm pos lst

    initialLogMap = snd (foldl (\(j, m) i -> (1 + j, insert i (newLog j) m)) (1, empty) is)

initial1 :: [Int]
initial1 = [14, 3, 1, 0, 9, 5]

solutionWith :: Int -> [Int] -> Int
solutionWith atPos is = fromInitial (atPos - length is) is

solution1 :: Int
solution1 = solutionWith 2020 initial1

solution2 :: Int
solution2 = solutionWith 30000000 initial1
