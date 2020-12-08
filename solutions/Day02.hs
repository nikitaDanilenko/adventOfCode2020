import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, letter, spaces)
import Text.Parsec.Language (haskellStyle)
import Text.ParserCombinators.Parsec.Token (makeTokenParser, integer)
import Text.Parsec (parse, many)
import Data.List (genericLength, genericDrop)

data Policy = Policy { minOccurrence :: Integer, maxOccurrence :: Integer, character :: Char }

isValid :: Policy -> String -> Bool
isValid policy str = n >= minOccurrence policy && n <= maxOccurrence policy where
    n = genericLength (filter (character policy ==) str)

isValid2 :: Policy -> String -> Bool
isValid2 policy str = 
    isAtPosition (minOccurrence policy) c str /= isAtPosition (maxOccurrence policy) c str where
        c = character policy

isAtPosition :: Eq a => Integer -> a -> [a] -> Bool
isAtPosition n x xs = if null rest then False else head rest == x
    where rest = genericDrop (n - 1) xs

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

haskellParser = makeTokenParser haskellStyle

int = integer haskellParser

readPolicy :: String -> (Policy, String)
readPolicy text =
    case parse policyParser "" text of
        Right p -> p
        Left err -> error "Could not parse policy"



readPolicies :: IO [(Policy, String)]
readPolicies = do
    pws <- readFile "./inputs/day02.txt"
    let policies = map readPolicy (lines pws)
    return policies
