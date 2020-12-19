module Day19 where

import Day02 (int)
import Day04 (splitOn)
import Day12 (runParser)
import Day18 (naturalParser, readLines)
import Text.Parsec (between, choice, many, many1, try)
import Text.Parsec.Char (anyChar, char, satisfy, spaces, string)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.String (Parser)
import Data.Char (isDigit)

data Rule = Terminal Integer Char | References Integer [[Integer]]
  deriving (Show)

terminalParser :: Parser Rule
terminalParser = do
  number <- naturalParser
  char ':'
  spaces
  char '"'
  c <- anyChar
  char '"'
  pure (Terminal number c)

listParser :: Parser [Integer]
listParser = sepBy naturalParser (char ' ')

referenceParser :: Parser Rule
referenceParser = do
  number <- naturalParser
  char ':'
  spaces
  rs <- fmap (map (map read) . splitOn "|" . words) (many1 (satisfy (\c -> isDigit c || c `elem` " |")))
  pure (References number rs)

ruleParser :: Parser Rule
ruleParser = choice [try referenceParser, terminalParser]

parseRule :: String -> Maybe Rule
parseRule = runParser ruleParser
