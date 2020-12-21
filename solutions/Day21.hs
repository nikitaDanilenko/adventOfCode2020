{-# LANGUAGE TupleSections #-}

module Day21 where

import           Control.Applicative                (liftA2)
import           Data.Char                          (isAlpha)
import           Data.Map                           (Map, elems, fromListWith)
import           Data.Maybe                         (mapMaybe)
import           Data.Set                           (Set, difference, fromList,
                                                     intersection, notMember,
                                                     union, unions)
import           Day12                              (runParser)
import           Day18                              (readLines)
import           Text.Parsec.Char                   (char, satisfy, string)
import           Text.Parsec.Combinator             (many1, sepBy, sepEndBy)
import           Text.Parsec.String                 (Parser)
import           Text.ParserCombinators.Parsec.Char (space, spaces)

word :: Parser String
word = many1 (satisfy isAlpha)

allergenParser :: Parser [String]
allergenParser = do
  char '('
  string "contains"
  spaces
  as <- word `sepBy` string ", "
  char ')'
  pure as

data Recipe = Recipe { ingredients :: [String], allergens :: [String]}
  deriving Show

recipeParser :: Parser Recipe
recipeParser = liftA2 Recipe (word `sepEndBy` space) allergenParser

readRecipes :: FilePath -> IO [Recipe]
readRecipes file = do
  ls <- readLines file
  pure (mapMaybe (runParser recipeParser) ls)

type AllergenMap = Map String (Set String)

mkMap :: [Recipe] -> AllergenMap
mkMap = fromListWith intersection . concatMap (\r -> map (, fromList (ingredients r)) (allergens r))

intersectAll :: Ord a => [Set a] -> Set a
intersectAll = foldr1 intersection

allIngredients :: [Recipe] -> Set String
allIngredients = fromList . concatMap ingredients

nonAllergens :: [Recipe] -> [String]
nonAllergens rs = filter (`notMember` as) (concatMap ingredients rs) where
  as = unions (elems (mkMap rs))
