{-# LANGUAGE TupleSections #-}

module Day14 where

import           Data.Char              (digitToInt, intToDigit)
import           Data.Map               (Map, elems, empty, fromList, insert,
                                         union)
import           Data.Maybe             (mapMaybe)
import           Day02                  (int)
import           Day12                  (runParser)
import           Numeric                (readInt, showIntAtBase)
import           Text.Parsec            (many, try)
import           Text.Parsec.Char       (anyChar, spaces, string)
import           Text.Parsec.Combinator (choice)
import           Text.Parsec.String     (Parser)

toBinary :: Integer -> String
toBinary t = showIntAtBase 2 intToDigit t ""

fromBinary :: String -> Integer
fromBinary = fst . head . readInt 2 (`elem` "01") digitToInt

padTo :: a -> [a] -> [a] -> [a]
padTo smb shorter = zipWith const (shorter ++ repeat smb)

process :: Char -> Char -> Char
process op c
  | op `elem` "01" = op
  | otherwise = c

applyBitMask :: String -> Integer -> Integer
applyBitMask bm =
  fromBinary . reverse . zipWith process (reverse bm) . flip (padTo '0') bm . reverse . toBinary

data Instruction = Mask String | Op {position :: Integer, value :: Integer}
  deriving (Show)

maskParser :: Parser Instruction
maskParser =
  string "mask"
    >> spaces
    >> string "="
    >> spaces
    >> fmap Mask (many anyChar)

memParser :: Parser Instruction
memParser = do
  _ <- string "mem["
  p <- int
  _ <- string "]"
  _ <- spaces
  _ <- string "="
  _ <- spaces
  Op p <$> int

instructionParser :: Parser Instruction
instructionParser = choice [try maskParser, memParser]

readInput :: FilePath -> IO [Instruction]
readInput f = do
  text <- readFile f
  let ls = lines text
      is = mapMaybe (runParser instructionParser) ls
  pure is

type Memory = Map Integer Integer

processInstructions :: (Memory -> String -> Instruction -> (Memory, String)) -> [Instruction] -> Integer
processInstructions single = sum . elems . fst . foldl (uncurry single) (empty, "")

single1 ::  Memory -> String -> Instruction -> (Memory, String)
single1 memory mask i = case i of
 Mask m   -> (memory, m)
 Op pos v -> (insert pos (applyBitMask mask v) memory, mask)

solution1 :: FilePath -> IO Integer
solution1 = fmap (processInstructions single1). readInput

process2 :: Char -> Char -> Char
process2 op c
  | op == '0' = c
  | op == '1' = '1'
  | otherwise = op

applyBitMask2 :: String -> Integer -> String
applyBitMask2 bm =
  reverse . zipWith process2 (reverse bm) . flip (padTo '0') bm . reverse . toBinary

pairwise :: [a] -> [[a]] -> [[a]]
pairwise xs yss = [x : ys | x <- xs, ys <- yss ]

pairwiseAll :: [[a]] -> [[a]]
pairwiseAll = foldr pairwise [[]]

alternatives :: String -> [[Char]]
alternatives = map step where
  step 'X' = "01"
  step c   = [c]

allPositionsWith :: String -> Integer -> [Integer]
allPositionsWith bm = map fromBinary . pairwiseAll . alternatives . applyBitMask2 bm

single2 :: Memory -> String -> Instruction -> (Memory, String)
single2 mem mask i = case i of
  Mask m -> (mem, m)
  Op pos v -> (u `union` mem, mask) where
    allMems = allPositionsWith mask pos
    u = fromList (map (, v) allMems)

solution2 :: String -> IO Integer
solution2 = fmap (processInstructions single2) .readInput
