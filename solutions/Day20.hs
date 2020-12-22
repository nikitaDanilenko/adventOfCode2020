module Day20 where

import           Data.Char  (isDigit)
import           Data.List  (find, intercalate, (\\), nub)
import           Data.Map   (Map, fromList, keys, size)
import qualified Data.Map   as M (lookup, filter)
import           Data.Maybe (fromJust, mapMaybe)
import           Day04      (splitOn)
import           Day18      (readLines)

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
rotateLeft (Tile n im) = Tile n (map (\j -> map (!! j) im) inds)
  where
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
anySideMatchesSimple t1 t2 =
  or
    [ rightCol t1 == leftCol t2,
      leftCol t1 == rightCol t2,
      topRow t1 == bottomRow t2,
      bottomRow t1 == topRow t2
    ]

anySideMatches :: Tile -> Tile -> Bool
anySideMatches t1 = any (anySideMatchesSimple t1) . allVariants

allVariants :: Tile -> [Tile]
allVariants t = [f (r t) | f <- [id, flipH, flipV, flipH . flipV], r <- map rotateLeftN [0 .. 3]]

findMatchingSides :: Tile -> [Tile] -> ([Tile], [Tile])
findMatchingSides t ts = (matching, ts \\ matching)
  where
    matching = filter (\t' -> number t' /= number t && anySideMatches t t') ts

findCorners :: [Tile] -> [(Tile, ([Tile], [Tile]))]
findCorners ts =
  filter ((== 2) . length . fst . snd) (map (\t -> (t, findMatchingSides t ts)) ts)

-- The corners have only two fitting neighbours; find the corners without computing the whole image.
solution1 :: [Tile] -> Integer
solution1 = product . map (number . fst) . findCorners

allPossibleTiles :: [Tile] -> [Tile]
allPossibleTiles = concatMap allVariants

nextRight :: Tile -> [Tile] -> Maybe Tile
nextRight current = find (\t -> number current /= number t && rightCol current == leftCol t)

nextBottom :: Tile -> [Tile] -> Maybe Tile
nextBottom current = find (\t -> number current /= number t && bottomRow current == topRow t)

data Line = Line {line :: [Tile], remainderLine :: [Tile]}
  deriving (Show)

nextLine :: Tile -> [Tile] -> Line
nextLine c ts = case nextRight c ts of
  Just t ->
    let l = nextLine t (differentNumber c ts)
     in Line {line = c : line l, remainderLine = differentNumber c (remainderLine l)}
  Nothing -> Line [c] (differentNumber c ts)

nextLines :: Tile -> [Tile] -> [[Tile]]
nextLines _ [] = []
nextLines t ts =
  let l = nextLine t ts
      r = remainderLine l
   in case nextBottom t r of
        Just b  -> line l : nextLines b r
        Nothing -> [line l]

verifyShow :: [[Tile]] -> String
verifyShow tts = intercalate "\n" (map verifyShowLine tts)
  where
    size = length (image (head (head tts)))
    verifyShowLine ts =
      unlines [unwords (map ((!! i) . image) ts) | i <- [0 .. size - 1]]

differentNumber :: Tile -> [Tile] -> [Tile]
differentNumber t = filter (\x -> number x /= number t)

isTopLeft :: Tile -> [Tile] -> Bool
isTopLeft t ts =
  let nonT = differentNumber t ts
   in case (nextRight t nonT, nextBottom t nonT) of
        (Just _, Just _) -> True
        _                -> False

computeImage :: [Tile] -> [[Tile]]
computeImage ts = nextLines corner (filter (\t -> number t /= number corner) allTs)
  where
    corner = fromJust (find (`isTopLeft` allTs) (allPossibleTiles (map fst (findCorners ts))))
    allTs = allPossibleTiles ts

removeBorders :: Tile -> Tile
removeBorders t = t {image = inner . map inner . image $ t}

inner :: [a] -> [a]
inner = tail . init

fuseH :: Tile -> Tile -> Tile
fuseH t1 t2 = t1 {image = zipWith (++) (image t1) (image t2)}

fuseHAll :: [Tile] -> Tile
fuseHAll = foldr1 fuseH

fuseV :: Tile -> Tile -> Tile
fuseV t1 t2 = t1 {image = image t1 ++ image t2}

fuseVAll :: [Tile] -> Tile
fuseVAll = foldr1 fuseV

mkCompleteTile :: [[Tile]] -> Tile
mkCompleteTile = fuseVAll . map (fuseHAll . map removeBorders)

type Pos = (Int, Int)

type CharGrid = Map Pos Char

mkMap :: Tile -> CharGrid
mkMap = fromList . concat . zipWith (\i ls -> zipWith (\j c -> ((i, j), c)) [0 :: Int ..] ls) [0 :: Int ..] . image

seaMonsterOffsets :: [Pos]
seaMonsterOffsets =
  [ (0, 18),
    (1, 0),
    (1, 5),
    (1, 6),
    (1, 11),
    (1, 12),
    (1, 17),
    (1, 18),
    (1, 19),
    (2, 1),
    (2, 4),
    (2, 7),
    (2, 10),
    (2, 13),
    (2, 16)
  ]

(.+.) :: Pos -> Pos -> Pos
(a, b) .+. (c, d) = (a + c, b + d)

seaMonsterPositionsFrom :: CharGrid -> Pos -> [Pos]
seaMonsterPositionsFrom cg p
  | length cs == length seaMonsterOffsets && all (== '#') cs = ps
  | otherwise = []
  where
    cs = mapMaybe (`M.lookup` cg) ps
    ps = map (p .+.) seaMonsterOffsets

findSeaMonsterPositions :: CharGrid -> [Pos]
findSeaMonsterPositions cg = nub (concatMap (seaMonsterPositionsFrom cg) (keys cg))

findNonSeaMonsterPositions :: CharGrid -> [Pos]
findNonSeaMonsterPositions cg = keys cg \\ findSeaMonsterPositions cg

hashSigns :: CharGrid -> Int
hashSigns = size . M.filter (== '#')

solution2 :: FilePath -> IO Int
solution2 file = do
  tiles <- readTiles file
  let ct = mkCompleteTile (computeImage tiles)
      cgs = map mkMap (allVariants ct)
      (cg, ps) = fromJust (find (not . null . snd) (map (\cgr -> (cgr, findSeaMonsterPositions cgr)) cgs))
      nonSeaMonsterPositions = hashSigns cg - length ps
  pure nonSeaMonsterPositions
