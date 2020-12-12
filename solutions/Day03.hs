module Day03 where

import           Data.Map (Map, fromList, (!))

data Field = E | T
  deriving (Show)

isTree :: Field -> Bool
isTree T = True
isTree _ = False

readField :: Char -> Field
readField '.' = E
readField _   = T

type Geography = Map (Int, Int) Field

mkMatrix :: String -> (Geography, Int, Int)
mkMatrix s = (m, height, width)
  where
    m = fromList (concat (zipWith (\i js -> map (\(j, f) -> ((i, j), f)) js) [1 ..] (map (zip [1 ..] . map readField) ls)))
    ls = lines s
    height = length ls
    width = length (head ls)

countIfTree :: Field -> Int
countIfTree f = if isTree f then 1 else 0

descendSteps :: Int -> Int -> Int -> Int -> Geography -> Int
descendSteps height width right down geo = go 0 (1, 1)
  where
    go counter (i, j)
      | i <= height = go (countIfTree (geo ! (i, j)) + counter) (i + down, 1 + (((j + right) - 1) `mod` width))
      | otherwise = counter

readGeography :: IO (Geography, Int, Int)
readGeography =
  fmap mkMatrix (readFile "../inputs/day03.txt")
