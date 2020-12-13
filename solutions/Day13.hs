module Day13 where

import Day04 ( splitOn )
import Data.List ( find )
import Data.Maybe (fromJust )



merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge l1@(x : xs) l2@(y : ys)
  | x == y = x : y : merge xs ys
  | x < y = x : merge xs l2
  | otherwise = y : merge l1 ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll = foldr merge []

mkMultiples :: Integer -> [Integer]
mkMultiples n = map (n *) [1 ..]

mkAllMultiples :: [Integer] -> [Integer]
mkAllMultiples = mergeAll . map mkMultiples

readInput1 :: String -> IO (Integer, [Integer])
readInput1 file = do
  text <- readFile file
  let (l1 : l2 : _) = lines text
      target = read l1 :: Integer
      numbers = map (\i -> read i :: Integer) (filter (/= "x") (splitOn ',' l2))
  pure (target, numbers)

solve1 :: Integer -> [Integer] -> Integer
solve1 target ns = (solution - target) * n where
  n = fromJust (find (\d -> solution `mod` d == 0) ns)
  solution = head (dropWhile (< target) (mkAllMultiples ns))

solution1 :: String -> IO Integer
solution1 = fmap (uncurry solve1) . readInput1

search :: [(Integer, Integer)] -> Integer
search withOffset = go 1 where
  go n | all (\(a, off) -> (n + off) `mod` a == 0) withOffset = n
       | otherwise = go (1 + n)

-- For coprime a, b finds c, d such that a*c + b*d = 1.
-- Implementation taken from https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
bezout :: Integer -> Integer -> (Integer, Integer)
bezout _ 0 = (1, 0)
bezout a b = (t, s - q * t)
  where
    (s, t) = bezout b r
    (q, r) = a `quotRem` b

-- Implementation mostly based upon the explicit solution given on Wikipedia.
chineseRemainder :: [(Integer, Integer)] -> Integer
chineseRemainder niais = t `mod` p
  where p = product (map fst niais)

        t = sum (map (\(ni, ai) -> let subP = p `div` ni in ai * subP * fst (bezout subP ni)) niais)


readInput2 :: String -> IO [(Integer, Integer)]
readInput2 file = do
  text <- readFile file
  let (_ : l2 : _) = lines text
      withOffsets = map (\(i, p) -> (read i :: Integer, p)) (filter ((/= "x") . fst) (zip (splitOn ',' l2) (map negate [0 ..])))
  pure withOffsets