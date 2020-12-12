module Day02 where

import           Data.List                           (genericDrop,
                                                      genericLength)
import           Text.Parsec                         (many, parse)
import           Text.Parsec.Char                    (char, letter, spaces)
import           Text.Parsec.Language                (haskellStyle)
import           Text.Parsec.String                  (Parser)
import           Text.Parsec.Token                   (TokenParser)
import           Text.ParserCombinators.Parsec.Token (integer, makeTokenParser)

data Policy = Policy {minOccurrence :: Integer, maxOccurrence :: Integer, character :: Char}

isValid :: Policy -> String -> Bool
isValid policy str = n >= minOccurrence policy && n <= maxOccurrence policy
  where
    n = genericLength (filter (character policy ==) str)

isValid2 :: Policy -> String -> Bool
isValid2 policy str =
  isAtPosition (minOccurrence policy) c str /= isAtPosition (maxOccurrence policy) c str
  where
    c = character policy

isAtPosition :: Eq a => Integer -> a -> [a] -> Bool
isAtPosition n x xs = not (null rest) && (head rest == x)
  where
    rest = genericDrop (n - 1) xs

policyParser :: Parser (Policy, String)
policyParser = do
  minOcc <- int
  char '-'
  maxOcc <- int
  spaces
  l <- letter
  spaces
  char ':'
  spaces
  pw <- many letter
  return (Policy minOcc maxOcc l, pw)

haskellParser :: TokenParser u
haskellParser = makeTokenParser haskellStyle

int :: Parser Integer
int = integer haskellParser

readPolicy :: String -> (Policy, String)
readPolicy text =
  case parse policyParser "" text of
    Right p -> p
    Left _  -> error "Could not parse policy"

readPolicies :: IO [(Policy, String)]
readPolicies = do
  pws <- readFile "../inputs/day02.txt"
  let policies = map readPolicy (lines pws)
  return policies
