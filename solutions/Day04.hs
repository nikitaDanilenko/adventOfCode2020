module Day04 where

import Data.Char (isDigit, isHexDigit)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (mapMaybe)
import Text.Parsec (many, parse)
import Text.Parsec.Char (char, noneOf, space, string)
import Text.Parsec.String (Parser)

data Passport = Passport
  { byr :: String,
    ecl :: String,
    eyr :: String,
    hcl :: String,
    hgt :: String,
    iyr :: String,
    pid :: String
  }
  deriving (Show)

passportParser :: Parser Passport
passportParser = do
  byrParam <- mkAttributeParser "byr"
  space
  eclParam <- mkAttributeParser "ecl"
  space
  eyrParam <- mkAttributeParser "eyr"
  space
  hclParam <- mkAttributeParser "hcl"
  space
  hgtParam <- mkAttributeParser "hgt"
  space
  iyrParam <- mkAttributeParser "iyr"
  space
  pidParam <- mkAttributeParser "pid"
  return (Passport byrParam eclParam eyrParam hclParam hgtParam iyrParam pidParam)

mkAttributeParser :: String -> Parser String
mkAttributeParser name = do
  string name
  char ':'
  many (noneOf " ")

readPassport :: Parser Passport -> [String] -> Maybe Passport
readPassport parser ls =
  case parse parser "" (unwords (filter (not . isPrefixOf "cid") (sort (words (unwords ls))))) of
    Right p -> Just p
    _ -> Nothing

isValidPassport :: Passport -> Bool
isValidPassport p =
  and [byrValid (byr p), eclValid (ecl p), eyrValid (eyr p), hclValid (hcl p), hgtValid (hgt p), iyrValid (iyr p), pidValid (pid p)]

yearValid :: Int -> Int -> String -> Bool
yearValid low high ys = all isDigit ys && low <= y && y <= high
  where
    y = read ys :: Int

byrValid :: String -> Bool
byrValid = yearValid 1920 2002

eclValid :: String -> Bool
eclValid e = e `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

eyrValid :: String -> Bool
eyrValid = yearValid 2020 2030

hclValid :: String -> Bool
hclValid [] = False
hclValid (c : cs) = c == '#' && length cs == 6 && all isHexDigit cs

hgtValid :: String -> Bool
hgtValid s = validSuffixWith "cm" 150 193 s || validSuffixWith "in" 59 76 s
  where
    validSuffixWith suffix low high s = isSuffixOf suffix s && low <= h && h <= high
      where
        h = read (takeWhile isDigit s) :: Int

iyrValid :: String -> Bool
iyrValid = yearValid 2010 2020

pidValid :: String -> Bool
pidValid s = all isDigit s && length s == 9

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn a = go
  where
    go [] = []
    go as = first : go (drop 1 last) where (first, last) = span (/= a) as

readPassports :: Parser Passport -> String -> [Passport]
readPassports p = mapMaybe (readPassport p) . splitOn "" . lines

readAllValid :: (Passport -> Bool) -> IO Int
readAllValid validator = do
  text <- readFile "../inputs/day04.txt"
  let valids = readPassports passportParser text
  return (length (filter validator valids))
