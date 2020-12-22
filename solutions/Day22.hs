module Day22 where

import Day18 (readLines)
import Day04 (splitOn)

readCards :: FilePath -> IO ([Integer], [Integer])
readCards file = do
  ls <- readLines file
  let f : s : _ = splitOn "" ls
      toList = map read . tail
  pure (toList f, toList s)

play :: [Integer] -> [Integer] -> [Integer]
play xs [] = xs
play [] ys = ys
play (x : xs) (y : ys)
  | x > y = play (xs ++ [x, y]) ys
  | x < y = play xs (ys ++ [y, x])
  | otherwise = error "Invalid input"

result :: [Integer] -> Integer
result = sum . zipWith (*) [1 ..] . reverse

solution1 :: FilePath -> IO Integer
solution1 = fmap (result . uncurry play) . readCards
