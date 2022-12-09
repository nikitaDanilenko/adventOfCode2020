module Day17 where

import Data.List ((\\))
import Data.Set ( Set, member, fromList, toList, delete, size )
import qualified Data.Set as S (filter, map)

data Point3 = Point3 {posX :: Int, posY :: Int, posZ :: Int}
  deriving (Show, Eq, Ord)

(.+.) :: Point3 -> Point3 -> Point3
Point3 ax ay az .+. Point3 bx by bz = Point3 (ax + bx) (ay + by) (az + bz)

neighbourOffsets3 :: [Point3]
neighbourOffsets3 = pointsInRanges3 (-1, 1) (-1, 1) (-1, 1) \\ [Point3 0 0 0]

neighbours3 :: Point3 -> [Point3]
neighbours3 p = map (p .+.) neighbourOffsets3

activeNeighbours :: Point3 -> [Point3] -> [Point3]
activeNeighbours p ps = filter (`elem` ps) (neighbours3 p)

pointsInRanges3 :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [Point3]
pointsInRanges3 (minX, maxX) (minY, maxY) (minZ, maxZ) =
  [Point3 x y z | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ]]

minMax :: Ord a => [a] -> (a, a)
minMax xs = (minimum xs, maximum xs)

minCube3 :: [Point3] -> [Point3]
minCube3 ps = pointsInRanges3 xRange yRange zRange
  where
    xRange = adjustedMinMax (map posX ps)
    yRange = adjustedMinMax (map posY ps)
    zRange = adjustedMinMax (map posZ ps)
    adjust (x, y) = (x - 1, y + 1)
    adjustedMinMax = adjust . minMax

step3 :: [Point3] -> [Point3]
step3 ps = filter activeNext (minCube3 ps)
  where
    activeNext p
      | p `elem` ps = length (activeNeighbours p ps) `elem` [2, 3]
      | otherwise = length (activeNeighbours p ps) == 3

readInput3 :: FilePath -> IO [Point3]
readInput3 file = do
  text <- readFile file
  let ls = lines text
      ps = concat (zipWith (map . Point3 0) [0 ..] (map (map fst . filter ((== '#') . snd) . zip [0 ..]) ls))
  pure ps

solution1 :: FilePath -> IO Int
solution1 file =
  fmap (length . (!! 6) . iterate step3) (readInput3 file)

data Point4 = Point4 {pos4W :: Int, pos4X :: Int, pos4Y :: Int, pos4Z :: Int}
  deriving (Show, Eq, Ord)

(.++.) :: Point4 -> Point4 -> Point4
Point4 aw ax ay az .++. Point4 bw bx by bz = Point4 (aw + bw) (ax + bx) (ay + by) (az + bz)

neighbourOffsets4 :: Set Point4
neighbourOffsets4 = delete (Point4 0 0 0 0) (pointsInRanges4 (-1, 1) (-1, 1) (-1, 1) (-1, 1))

neighbours4 :: Point4 -> Set Point4
neighbours4 p = S.map (p .++.) neighbourOffsets4

activeNeighbours4 :: Point4 -> Set Point4 -> Set Point4
activeNeighbours4 p ps = S.filter (`member` ps) (neighbours4 p)

pointsInRanges4 :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Set Point4
pointsInRanges4 (minW, maxW) (minX, maxX) (minY, maxY) (minZ, maxZ) =
  fromList [Point4 w x y z | w <- [minW .. maxW], x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ]]

minCube4 :: Set Point4 -> Set Point4
minCube4 ps = pointsInRanges4 wRange xRange yRange zRange
  where
    wRange = adjustedMinMax (S.map pos4W ps)
    xRange = adjustedMinMax (S.map pos4X ps)
    yRange = adjustedMinMax (S.map pos4Y ps)
    zRange = adjustedMinMax (S.map pos4Z ps)
    adjust (x, y) = (x - 1, y + 1)
    adjustedMinMax = adjust . minMax . toList

step4 :: Set Point4 -> Set Point4
step4 ps = S.filter activeNext (minCube4 ps)
  where
    activeNext p
      | p `elem` ps = length (activeNeighbours4 p ps) `elem` [2, 3]
      | otherwise = length (activeNeighbours4 p ps) == 3

readInput4 :: FilePath -> IO (Set Point4)
readInput4 file = do
  text <- readFile file
  let ls = lines text
      ps = concat (zipWith (map . Point4 0 0) [0 ..] (map (map fst . filter ((== '#') . snd) . zip [0 ..]) ls))
  pure (fromList ps)

solution2 :: FilePath -> IO Int
solution2 file =
  fmap (size . (!! 6) . iterate step4) (readInput4 file)