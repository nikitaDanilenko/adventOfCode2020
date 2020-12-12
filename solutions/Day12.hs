module Day12 where

import Day02 (int)
import Text.Parsec (choice, parse)
import Text.Parsec.Char (char, string)
import Text.Parsec.String (Parser)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))

data Direction = E | N | W | S
  deriving (Show, Read)

rotate :: Direction -> Direction
rotate E = N
rotate N = W
rotate W = S
rotate S = E

data Instruction = Rotation Integer | Move Direction Integer | Forward Integer
  deriving (Show)

parserRotation :: Parser Instruction
parserRotation = do
  c <- choice [char 'L', char 'R']
  i <- int
  pure (Rotation (degreesToInt (c : show i)))

parserDirection :: Parser Instruction
parserDirection = do c <- fmap read (choice (map (string . show) [E, N, W, S]))
                     Move c <$> int

parserForward :: Parser Instruction
parserForward = do char 'F'
                   Forward <$> int

-- Does not terminate. Why?
parserInstruction :: Parser Instruction
parserInstruction = choice [parserRotation, parserInstruction, parserForward]

runParser :: Parser a -> String -> Maybe a
runParser p s = case parse p "" s of
  Left _ -> Nothing
  Right a -> Just a

parseInstruction :: String -> Maybe Instruction
parseInstruction str =
  runParser parserRotation str <|> runParser parserDirection str <|> runParser parserForward str

readInstructions :: String -> IO [Instruction]
readInstructions file = do
  text <- readFile file
  let ls = lines text
  pure (mapMaybe parseInstruction ls)

rotateInt :: Integer -> Direction -> Direction
rotateInt n d = iterate rotate d !! fromIntegral n

degreesToInt :: String -> Integer
degreesToInt str
  | str `elem` ["R90", "L270"] = 3
  | str `elem` ["R180", "L180"] = 2
  | otherwise = 1

move :: Instruction -> Pos -> Direction -> (Pos, Direction)
move ins (x, y) facing =
  case ins of
    Rotation i -> ((x, y), rotateInt i facing)
    Move d i -> (step d i (x, y), facing)
    Forward i -> (step facing i (x, y), facing)

type Pos = (Integer, Integer)

data Positions = Positions {
  ofShip :: Pos,
  waypointDiff :: Pos
 } deriving Show

waypointOffset :: Pos
waypointOffset = (1, 10)

moveWithWaypoint :: Instruction -> Positions -> Positions
moveWithWaypoint ins ps = case ins of
  Rotation i -> rotateWithWaypoint i ps
  Move d i -> ps { waypointDiff = step d i (waypointDiff ps)}
  Forward i -> moveToWaypoint i ps

(.+.) :: Pos -> Pos -> Pos
(x, y) .+. (a, b) = (x + a, y + b)

moveToWaypoint :: Integer -> Positions -> Positions
moveToWaypoint i ps = stepTo i off ps where
  off = waypointDiff ps

  stepTo i off ps | i <= 0 = ps
                  | otherwise = stepTo (i - 1) off (ps { ofShip = ofShip ps .+. off})

rotateWithWaypoint :: Integer -> Positions -> Positions
rotateWithWaypoint i ps = Positions os ow where
  (n, e) = waypointDiff ps
  ow | i == 1 = (e, -n)
     | i == 2 = (-n, -e)
     | otherwise = (-e, n)

  os = ofShip ps

step :: Direction -> Integer -> Pos -> Pos
step d i (x, y) = case d of
  E -> (x, y + i)
  W -> (x, y - i)
  N -> (x + i, y)
  S -> (x - i, y)

proceed :: [Instruction] -> Pos
proceed = fst . foldl (\(p, facing) ins  -> move ins p facing) ((0, 0), E)

solution1 :: String -> IO Integer
solution1 file = do
 is <- readInstructions file
 let (x, y) = proceed is
 pure (abs x + abs y)

proceed2 :: [Instruction] -> Pos
proceed2 = ofShip . foldl (flip moveWithWaypoint) (Positions (0, 0) waypointOffset)