module Day20 where

import Data.Char (isDigit)
import Day18 (readLines)
import Day04 (splitOn)
import Data.List ( (\\), transpose )

data Tile = Tile {number :: Integer, image :: [String]}
  deriving (Show, Eq)

readTile :: [String] -> Tile
readTile str =
  Tile (readNumber (head str)) (tail str)

readNumber :: String -> Integer
readNumber = read . takeWhile isDigit . dropWhile (not . isDigit)

flipH :: Tile -> Tile
flipH (Tile n i) = Tile n (map reverse i)

flipV :: Tile -> Tile
flipV (Tile n i) = Tile n (reverse i)

rotateLeft :: Tile -> Tile
rotateLeft (Tile n im) = Tile n (map (\j -> map (!! j) im) inds) where
  inds = reverse [0 .. length im - 1]

rotateLeftN :: Int -> Tile -> Tile
rotateLeftN n = (!! n) . iterate rotateLeft

leftCol :: Tile -> String
leftCol = map head . image

rightCol :: Tile -> String
rightCol = map last . image

topRow :: Tile -> String
topRow = head . image

bottomRow :: Tile -> String
bottomRow = last . image

readTiles :: FilePath -> IO [Tile]
readTiles file = do
  ls <- readLines file
  let gs = splitOn "" ls
      ts = map readTile gs
  pure ts

anySideMatchesSimple :: Tile -> Tile -> Bool
anySideMatchesSimple t1 t2 = or [
  rightCol t1 == leftCol t2,
  leftCol t1 == rightCol t2,
  topRow t1 == bottomRow t2,
  bottomRow t1 == topRow t2
  ]

anySideMatches :: Tile -> Tile -> Bool
anySideMatches t1 = any (anySideMatchesSimple t1) . allVariants

allVariants :: Tile -> [Tile]
allVariants t = [f (r t) | f <- [id, flipH, flipV, flipH . flipV], r <- map rotateLeftN [0 .. 3]]

findMatchingSides :: Tile -> [Tile] -> ([Tile], [Tile])
findMatchingSides t ts = (matching, ts \\ matching) where
  matching = filter (\t' -> number t' /= number t && anySideMatches t t') ts

findCorners :: [Tile] -> [(Tile, ([Tile], [Tile]))]
findCorners ts =
 filter ((== 2) . length . fst . snd) (map (\ t -> (t, findMatchingSides t ts)) ts)

-- The corners have only two fitting neighbours; find the corners without computing the whole image.
solution1 :: [Tile] -> Integer
solution1 = product . map (number . fst) . findCorners