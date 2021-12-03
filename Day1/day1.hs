module Main where

main = do
    txt <- readFile "day1.txt"
    let nums = read <$> lines txt
    print (increases nums)
    print (increasesThree nums)

-- count the number of increases from element to element
increases :: [Int] -> Int
increases [] = 0
increases [x,y]
    | x < y = 1
    | otherwise = 0
increases (x:y:xs)
    | x < y = 1 + increases(y:xs)
    | otherwise = increases(y:xs)

-- count increases in three unit windows
increasesThree :: [Int] -> Int
increasesThree all@(w:_:_:z:xs)
    | w < z = 1 + increasesThree(tail all)
    | otherwise = increasesThree(tail all)
increasesThree _ = 0
