module Day10 where

import           Data.List (sort)
import           Day04     (splitOn)

readInput :: IO [Integer]
readInput = do
  text <- readFile "../inputs/day10.txt"
  pure (map read (lines text))

data Jolts = Jolts {ones :: Integer, threes :: Integer}
  deriving (Show)

joltsProduct :: Jolts -> Integer
joltsProduct j = ones j * threes j

computeJolts :: [Integer] -> Jolts
computeJolts xs = jolts
  where
    jolts = foldr nextJolt (Jolts 1 1) pairs

    nextJolt :: (Integer, Integer) -> Jolts -> Jolts
    nextJolt (p, q) j
      | q - p == 3 = j {threes = 1 + threes j}
      | q - p == 1 = j {ones = 1 + ones j}
      | otherwise = j

    pairs = zip sorted (tail sorted)
    sorted = sort xs

tinyExample :: [Integer]
tinyExample = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

mediumExample :: [Integer]
mediumExample = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

solution1 :: IO Integer
solution1 = fmap (joltsProduct . computeJolts) readInput

diffGroups :: [Integer] -> [[Integer]]
diffGroups xs = splitOn 3 (zipWith (flip (-)) xs (tail xs))

-- This is only correct for differences of 1.
-- For more differences one can either use a brute force attempt or come up with a rule.
-- For combinations with a difference of 2 the situation becomes even more complex.
partialGuess :: Int -> Integer
partialGuess n
  | n <= 1 = 1
  | n == 2 = 2
  | n == 3 = 4
  | n == 4 = 7

-- The initial zero is important, because otherwise the first group assumes a distance of 3 to the outlet.
solution2 :: IO Integer
solution2 = fmap (product . map (partialGuess . length) . filter (not . null) . diffGroups . sort . (0 :)) readInput
