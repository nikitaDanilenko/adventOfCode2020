module Day07 where

import           Data.List              (groupBy, sortBy)
import           Data.Map               (Map, elems, fromList, keysSet)
import qualified Data.Map               as M (lookup)
import           Data.Maybe             (fromMaybe, mapMaybe)
import           Data.Ord               (comparing)
import qualified Data.Set               as S (difference, fromList,
                                              intersection, toList, union)
import           Day02                  (int)
import           Text.Parsec            (choice, parse, try)
import           Text.Parsec.Char       (anyChar, spaces, string)
import           Text.Parsec.Combinator (manyTill, optional, sepBy)
import           Text.Parsec.String     (Parser)

data Rule = Rule
  { ruleColour :: String,
    contents   :: [Content]
  }
  deriving (Show)

data Content = Content
  { amount        :: Integer,
    contentColour :: String
  }
  deriving (Show)

contentParser :: Parser Content
contentParser = do
  n <- int
  spaces
  col <- manyTill anyChar (try (string " bag"))
  optional (string "s")
  pure (Content n col)

ruleParser :: Parser Rule
ruleParser = do
  rcol <- manyTill anyChar (try (string " bags contain"))
  spaces
  cs <- choice [fmap (const []) (string "no other bags"), sepBy contentParser (string ", ")]
  string "."
  pure (Rule rcol cs)

rulesParser :: Parser [Rule]
rulesParser = sepBy ruleParser (string "\n")

type Graph a = Map a [a]

mkReverseGraph :: [Rule] -> Graph String
mkReverseGraph =
  fromList
    . map (\xs -> (fst (head xs), map snd xs))
    . groupBy (\(p, _) (q, _) -> p == q)
    . sortBy (comparing fst)
    . mkReversePairs
  where
    mkReversePairs = concatMap (\r -> map (\c -> (contentColour c, ruleColour r)) (contents r))

type ContentMap = Map String [Content]

mkContentMap :: [Rule] -> ContentMap
mkContentMap = fromList . map (\r -> (ruleColour r, contents r))

-- It is important to take all nodes into account, since the original graph may contain dead ends.
reachable :: Ord a => [a] -> Graph a -> [[a]]
reachable xs graph = go (S.union (keysSet graph) (S.fromList (concat (elems graph)))) xs
  where
    go unseen current
      | null current = []
      | otherwise = current : go newUnseen (S.toList newCurrent)
      where
        stepFull = concat (mapMaybe (`M.lookup` graph) current)
        newUnseen = S.difference unseen (S.fromList current)
        newCurrent = S.intersection (S.fromList stepFull) newUnseen

readRules :: IO [Rule]
readRules = do
  text <- readFile "../inputs/day07.txt"
  let rules = case parse rulesParser "" text of
        Right rs -> return rs
        Left err -> print err >> return []
  rules

solution1With :: [Rule] -> Int
solution1With = pred . length . concat . reachable ["shiny gold"] . mkReverseGraph

at :: ContentMap -> String -> [Content]
at cm c = fromMaybe [] (M.lookup c cm)

forColour :: ContentMap -> String -> Integer
forColour m c =
  sum (map (\ct -> amount ct * (1 + forColour m (contentColour ct)) * (1 + forColour m (contentColour ct))) (m `at` c))

solution2 :: IO Integer
solution2 = do
  rs <- readRules
  let cm = mkContentMap rs
      a = forColour cm "shiny gold"
  return a
