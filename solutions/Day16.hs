module Day16 where

import Data.List (findIndices, sortBy, transpose, (\\))
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Day04 (splitOn)

data Interval a = Interval {bottom :: a, top :: a}

inInterval :: Ord a => a -> Interval a -> Bool
inInterval x i = bottom i <= x && x <= top i

inAnyInterval :: Ord a => a -> [Interval a] -> Bool
inAnyInterval = any . inInterval

allInAnyInterval :: Ord a => [a] -> [Interval a] -> Bool
allInAnyInterval xs is = all (\x -> inAnyInterval x is) xs

intervals :: [Interval Int]
intervals = concat groupedIntervals

groupedIntervals :: [[Interval Int]]
groupedIntervals =
  [ [Interval 49 848, Interval 871 949],
    [Interval 33 670, Interval 687 969],
    [Interval 41 909, Interval 916 974],
    [Interval 40 397, Interval 422 972],
    [Interval 31 481, Interval 505 960],
    [Interval 37 299, Interval 312 965],
    [Interval 46 114, Interval 126 967],
    [Interval 28 453, Interval 478 963],
    [Interval 26 756, Interval 781 973],
    [Interval 30 231, Interval 252 968],
    [Interval 26 820, Interval 828 967],
    [Interval 31 901, Interval 910 958],
    [Interval 47 711, Interval 722 952],
    [Interval 48 518, Interval 524 956],
    [Interval 50 166, Interval 172 974],
    [Interval 26 792, Interval 810 963],
    [Interval 28 617, Interval 637 952],
    [Interval 30 734, Interval 748 962],
    [Interval 41 429, Interval 454 968],
    [Interval 25 129, Interval 142 971]
  ]

readInputs :: IO [[Int]]
readInputs = do
  text <- readFile "inputs/day16.txt"
  let ls = lines text
      iss = map (map read . splitOn ',') ls
  pure iss

solution1 :: (Ord a, Num a) => [[a]] -> [Interval a] -> a
solution1 ls is =
  sum (concatMap (filter (not . flip inAnyInterval is)) ls)

validTickets :: [[Int]] -> [[Int]]
validTickets = filter (flip allInAnyInterval intervals)

validPosition :: [Int] -> [Int]
validPosition xs = findIndices (\i -> all (flip inAnyInterval i) xs) groupedIntervals

ownTicket :: [Int]
ownTicket = [163, 73, 67, 113, 79, 101, 109, 149, 53, 61, 97, 89, 103, 59, 71, 83, 151, 127, 157, 107]

findPositions :: [[Int]] -> [(Int, Int)]
findPositions =
  sortBy (comparing fst)
    . map swap
    . fst
    . foldl (\(ps, seen) (i, xs) -> let h = head (xs \\ seen) in ((i, h) : ps, h : seen)) ([], [])
    . sortBy (comparing (length . snd))
    . zip [1 ..]
    . map validPosition
    . transpose
    . (ownTicket :)
    . validTickets

solution2 :: IO Integer
solution2 =
  (fmap (product . map (fromIntegral . (\i -> ownTicket !! (i - 1)) . snd) . take 6 . findPositions) readInputs)
