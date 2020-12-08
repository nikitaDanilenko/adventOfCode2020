module Day06 where

import Day04 ( splitOn )
import Data.Set ( Set, fromList, intersection, size )

countGroup :: [String] -> Int
countGroup = size . fromList . concat

countGroups :: [[String]] -> Int
countGroups = sum . map countGroup

solution1 :: IO Int
solution1 = do
    text <- readFile "day06input.txt"
    let gs = splitOn "" (lines text)
    return (countGroups gs)

countGroup2 :: [String] -> Int
countGroup2 = size . foldr1 intersection . map fromList