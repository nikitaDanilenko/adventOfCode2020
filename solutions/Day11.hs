module Day11 where

import Data.Map ( Map, fromList, toList, mapWithKey, size, keys, (!) )
import qualified Data.Map as M ( lookup, filter )
import Data.Maybe ( catMaybes )
import Data.List (sortBy, groupBy, sort, find, minimumBy)
import Data.Ord (comparing)
import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA512)
import qualified Data.ByteString.Char8               as B
import Data.Set (Set, insert, member, empty)

data Pos = Floor | Empty | Taken
    deriving Show

isEmpty :: Pos -> Bool
isEmpty Empty = True
isEmpty _ = False

isTaken :: Pos -> Bool
isTaken Taken = True
isTaken _ = False

isSeat :: Pos -> Bool
isSeat Floor = False
isSeat _ = True

floorSymbol, emptySymbol, takenSymbol :: Char
floorSymbol = '.'
emptySymbol = 'L'
takenSymbol = '#'

fromSymbol :: Char -> Pos
fromSymbol c
  | c == floorSymbol = Floor
  | c == emptySymbol = Empty
  | otherwise        = Taken

toSymbol :: Pos -> Char
toSymbol Floor = floorSymbol
toSymbol Empty = emptySymbol
toSymbol Taken = takenSymbol

type SeatMap = Map (Int, Int) Pos

mkMap :: String -> SeatMap
mkMap = fromList . concat . zipWith (\i ps -> zipWith (\j p -> ((i, j), p)) [0 ..] ps) [0 ..] . map (map fromSymbol) . lines

offsetPairs :: [(Int, Int)]
offsetPairs = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= b || a /= 0]

-- The neighbours should be computed only once, but since adding offsets is very cheap, the below implementation is fine.
neighbours :: SeatMap -> (Int, Int) -> [Pos]
neighbours sm (i, j) = catMaybes [M.lookup (i + a, j + b) sm | (a, b) <- offsetPairs]

type Visible = Map (Int, Int) [(Int, Int)]

mkVisible :: SeatMap -> Visible
mkVisible sm = mapWithKey (\p _ -> visibleFor p) sm
  where
    smIndices = keys sm
    visibleFor (i, j) = catMaybes (map (firstDefinedWith sm) [ups, downs, lefts, rights, topLefts, topRights, bottomLefts, bottomRights]) where
      ups = filter (\(a, b) -> a < i && b == j) smIndices
      downs = filter (\(a, b) -> a > i && b == j) smIndices
      lefts = filter (\(a, b) -> a == i && b < j) smIndices
      rights = filter (\(a, b) -> a == i && b > j) smIndices
      topLefts = filter (\(a, b) -> a < i && b < j && b - a == j - i) smIndices
      topRights = filter (\(a, b) -> a < i && b > j && i - a == b - j) smIndices
      bottomLefts = filter (\(a, b) -> a > i && b < j && a - i == j - b) smIndices
      bottomRights = filter (\(a, b) -> a > i && b > j && b - a == j - i) smIndices

      distanceTo (a, b) = abs (a - i) + abs (b - j)

      firstDefinedWith sm inds =
        let candidates = filter (\p -> isSeat (sm ! p)) inds
        in if null candidates then Nothing else Just (minimumBy (comparing distanceTo) candidates)

neighbours2 :: Visible -> SeatMap -> (Int, Int) -> [Pos]
neighbours2 vs sm (i, j) = map (sm !) (vs ! (i, j))

applyRules :: Int -> Pos -> [Pos] -> Pos
applyRules allowed p ns
  | isEmpty p && all (not . isTaken) ns = Taken
  | isTaken p && length (filter isTaken ns) >= allowed = Empty
  | otherwise = p

applyRulesAll :: Int -> (SeatMap -> (Int, Int) -> [Pos]) -> SeatMap -> SeatMap
applyRulesAll allowed nf sm =
  mapWithKey (\(i, j) p -> applyRules allowed p (nf sm (i, j))) (M.filter isSeat sm)

step :: Int -> (SeatMap -> (Int, Int) -> [Pos]) -> SeatMap -> Set String -> (SeatMap, Set String)
step a nf sm seen = (applyRulesAll a nf sm, hashOf sm `insert` seen)

fixpointSize :: Int -> (SeatMap -> (Int, Int) -> [Pos]) -> SeatMap -> Int
fixpointSize a nf sm =
  size
    $ M.filter isTaken
    $ fst
    $ head
    $ dropWhile (\(sm, set) -> not (hashOf sm `member` set))
    $ iterate (uncurry (step a nf)) (sm, empty)

readSeatMap :: IO SeatMap
readSeatMap = fmap mkMap (readFile "inputs/day11.txt")

solution1 :: IO Int
solution1 = fmap (fixpointSize 4 neighbours) readSeatMap

solution2 :: IO Int
solution2 = fmap (\sm -> fixpointSize 5 (neighbours2 (mkVisible sm)) sm) readSeatMap

type Hashed = Digest SHA512

mkHashed :: B.ByteString -> Hashed
mkHashed = hash

hashOf :: SeatMap -> String
hashOf =
    show
  . mkHashed
  . B.pack
  . unlines
  . map (map (toSymbol . snd))
  . groupBy (\x y -> fst x == fst y)
  . sortBy (comparing fst)
  . toList


