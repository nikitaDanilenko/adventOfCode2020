module Day22 where

import Data.List (genericLength, genericTake)
import Data.Set (Set, empty, insert, member)
import Day04 (splitOn)
import Day18 (readLines)

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

type Constellation = ([Integer], [Integer])

data Player = One | Two

play2 :: Set Constellation -> [Integer] -> [Integer] -> (Player, [Integer])
play2 _ [] ys = (Two, ys)
play2 _ xs [] = (One, xs)
play2 cs p1@(x : xs) p2@(y : ys)
  | (p1, p2) `member` cs = (One, p1)
  | genericLength xs >= x && genericLength ys >= y =
    case fst (play2 empty (genericTake x xs) (genericTake y ys)) of
          One -> play2 nextSet (xs ++ [x, y]) ys
          Two -> play2 nextSet xs (ys ++ [y, x])
  | x > y = play2 nextSet (xs ++ [x, y]) ys
  | x < y = play2 nextSet xs (ys ++ [y, x])
  | otherwise = error "Invalid input"
 where nextSet = (p1, p2) `insert` cs
 
solution2 :: FilePath -> IO Integer
solution2 = fmap (result . snd . uncurry (play2 empty)) . readCards