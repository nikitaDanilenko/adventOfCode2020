module Day09 where

import           Data.List  (find, inits, tails)
import           Data.Maybe (listToMaybe, mapMaybe)

readNumbers :: IO [Integer]
readNumbers =
  fmap (map read . lines) (readFile "../inputs/day09.txt")

check :: Int -> [Integer] -> Maybe Integer
check b is = result
  where
    (front, back) = splitAt b is
    result = case back of
      [] -> Nothing
      (x : _) -> if null [(m, n) | m <- front, n <- front, m /= n && m + n == x] then Just x else Nothing

checkAll :: Int -> [Integer] -> Maybe Integer
checkAll b = listToMaybe . mapMaybe (check b) . tails

solution1 :: IO (Maybe Integer)
solution1 = fmap (checkAll 25) readNumbers

matches :: Integer -> [Integer] -> Maybe [Integer]
matches target is =
  find (\xs -> sum xs == target) (inits is)

attemptMatch :: Integer -> [Integer] -> Maybe [Integer]
attemptMatch target =
  listToMaybe . mapMaybe (matches target) . tails

solution2 :: IO (Maybe Integer)
solution2 = do
  numbers <- readNumbers
  invalid <- solution1
  let result = do
        target <- invalid
        match <- attemptMatch target numbers
        let (l, h) = (minimum match, maximum match)
        pure (l + h)
  pure result
