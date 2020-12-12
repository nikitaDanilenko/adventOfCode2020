module Day05 where

data Position = Lower | Upper

asNumber :: [Position] -> Integer
asNumber ps = go 0 (2 ^ l - 1) ps
  where
    l = length ps

    go _ upper [] = upper
    go lower upper (p : ps) =
      let halfInterval = (1 + upper - lower) `div` 2
       in case p of
            Lower -> go lower (upper - halfInterval) ps
            Upper -> go (lower + halfInterval) upper ps

readRow :: String -> Integer
readRow = asNumber . map (asPosition 'F' 'B')

readColumn :: String -> Integer
readColumn = asNumber . map (asPosition 'L' 'R')

readSeat :: String -> (Integer, Integer)
readSeat s = (readRow front, readColumn back)
  where
    front = take 7 s
    back = drop 7 s

idOfSeat :: Integer -> Integer -> Integer
idOfSeat row col = 8 * row + col

asPosition :: Char -> Char -> Char -> Position
asPosition l u c
  | l == c = Lower
  | u == c = Upper

solution :: IO Integer
solution = do
  text <- readFile "../inputs/day05.txt"
  let ls = lines text
      ids = map (uncurry idOfSeat . readSeat) ls
  return (maximum ids)

-- The first one is the base, so add 1 afterwards.
findMissing :: [Integer] -> [Integer]
findMissing xs =
  map fst (filter (\(x, y) -> y > 1 + x) (zip xs (tail xs)))
