module Day11 where

import Data.Map ( Map, fromList, toList, mapWithKey, size )
import qualified Data.Map as M ( lookup, filter )
import Data.Maybe ( catMaybes )
import Data.List (sortBy, groupBy)
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

neighbours :: SeatMap -> (Int, Int) -> [Pos]
neighbours sm (i, j) = catMaybes [M.lookup (i + a, j + b) sm | (a, b) <- offsetPairs]

applyRules :: Pos -> [Pos] -> Pos
applyRules p ns
  | isEmpty p && all (not . isTaken) ns = Taken
  | isTaken p && length (filter isTaken ns) >= 4 = Empty
  | otherwise = p

applyRulesAll :: SeatMap -> SeatMap
applyRulesAll sm =
  mapWithKey (\(i, j) p -> applyRules p (neighbours sm (i, j))) sm

step :: SeatMap -> Set String -> (SeatMap, Set String)
step sm seen = (applyRulesAll sm, hashOf sm `insert` seen)

fixpointSize :: SeatMap -> Int
fixpointSize sm =
  size
    $ M.filter isTaken
    $ fst
    $ head
    $ dropWhile (\(sm, set) -> not (hashOf sm `member` set))
    $ iterate (uncurry step) (sm, empty)

readSeatMap :: IO SeatMap
readSeatMap = fmap mkMap (readFile "inputs/day11.txt")

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


