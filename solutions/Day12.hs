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

move :: Instruction -> (Integer, Integer) -> Direction -> ((Integer, Integer), Direction)
move ins (x, y) facing =
  case ins of
    Rotation i -> ((x, y), rotateInt i facing)
    Move d i -> (step d i (x, y), facing)
    Forward i -> (step facing i (x, y), facing)

step :: Direction -> Integer -> (Integer, Integer) -> (Integer, Integer)
step d i (x, y) = case d of
  E -> (x, y + i)
  W -> (x, y - i)
  N -> (x + i, y)
  S -> (x - i, y)

proceed :: [Instruction] -> (Integer, Integer)
proceed = fst . foldl (\(p, facing) ins  -> move ins p facing) ((0, 0), E)