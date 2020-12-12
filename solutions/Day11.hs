module Day11 where

import Data.Map ( Map, fromList )
import qualified Data.Map as M ( lookup )
import Data.Maybe ( catMaybes )
import Data.Hashable (hash)

data Pos = Floor | Empty | Taken
    deriving Show

fromSymbol :: Char -> Pos
fromSymbol '.' = Floor
fromSymbol 'L' = Empty
fromSymbol '#' = Taken

type SeatMap = Map (Int, Int) Pos

mkMap :: String -> SeatMap
mkMap = fromList . concat . zipWith (\i ps -> zipWith (\j p -> ((i, j), p)) [0 ..] ps) [0 ..] . map (map fromSymbol) . lines

offsetPairs :: [(Int, Int)]
offsetPairs = [(a, b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= b || a /= 0]

neighbours :: SeatMap -> (Int, Int) -> [Pos]
neighbours sm (i, j) = catMaybes [M.lookup (i + a, j + b) sm | (a, b) <- offsetPairs]