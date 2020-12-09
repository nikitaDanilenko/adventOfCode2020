module Day07 where

import Day02                   ( int )
import Text.Parsec.String      ( Parser )
import Text.Parsec.Char        ( spaces, anyChar, string )
import Text.Parsec.Combinator  ( manyTill, sepBy, optional )
import Text.Parsec             ( try, choice, parse )
import Data.Map                ( Map, fromList, keysSet, elems )
import Data.List               ( sortBy, groupBy )
import Data.Ord                ( comparing )
import qualified Data.Map as M ( lookup )
import Data.Maybe              ( catMaybes )
import qualified Data.Set as S ( fromList, intersection, difference, empty, toList, union ) 

data Rule = Rule {
    ruleColour :: String,
    contents :: [Content]
} deriving Show

data Content = Content {
    amount :: Integer,
    contentColour :: String
} deriving Show

contentParser :: Parser Content
contentParser = do
    n <- int
    spaces
    col <- manyTill anyChar (try (string " bag"))
    optional (string "s")
    return (Content n col)

ruleParser :: Parser Rule
ruleParser = do
    rcol <- manyTill anyChar (try (string " bags contain"))
    spaces
    cs <- choice [fmap (const []) (string "no other bags"), sepBy contentParser (string ", ")]
    string "."
    return (Rule rcol cs)

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
    where mkReversePairs = concatMap (\r -> map (\c -> (contentColour c, ruleColour r)) (contents r))

mkGraph :: [Rule] -> Map String Content
mkGraph = fromList . map (\r -> (ruleColour r, contents r))

-- It is important to take all nodes into account, since the original graph may contain dead ends.
reachable :: Ord a => [a] -> Graph a -> [[a]]
reachable xs graph = go (S.union (keysSet graph) (S.fromList (concat (elems graph)))) xs where
    go unseen current 
        | null current = []
        | otherwise = current : go newUnseen (S.toList newCurrent)
        where stepFull = concat (catMaybes (map (flip M.lookup graph) current))
              newUnseen = (S.difference unseen (S.fromList current))
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