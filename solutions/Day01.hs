allFitting2 :: Int -> [Int] -> [((Int, Int), Int)]
allFitting2 target vs = [((x, y), x * y) | x <- vs, y <- vs, x + y == target]

allFitting3 :: Int -> [Int] -> [((Int, Int, Int), Int)]
allFitting3 target vs = [((x, y, z), x * y * z) | x <- vs, y <- vs, z <- vs, x + y + z == target]

readValues :: IO [Int]
readValues = do
    text <- readFile "../inputs/day01.txt"
    let ls = lines text
    return (map read ls)