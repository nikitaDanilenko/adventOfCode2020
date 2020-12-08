module Day06 where

import Day04 ( splitOn )
import Data.Set ( Set, fromList, intersection, size )

countGroup :: [String] -> Int
countGroup = size . fromList . concat

countGroup2 :: [String] -> Int
countGroup2 = size . foldr1 intersection . map fromList

countGroupsWith :: ([String] -> Int) -> [[String]] -> Int
countGroupsWith cg = sum . map cg

solutionWith :: ([String] -> Int) -> IO Int
solutionWith cg = do
    text <- readFile "../inputs/day06.txt"
    let gs = splitOn "" (lines text)
    return (countGroupsWith cg gs)


solution1 :: IO Int
solution1 = solutionWith countGroup

solution2 :: IO Int
solution2 = solutionWith countGroup2