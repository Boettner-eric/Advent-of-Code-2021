module Main where

main = do
    txt <- readFile "day1.txt"
    let nums = read <$> lines txt -- read input, split on \n, convert to [Int]
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
increasesThree [] = 0
increasesThree [_,_] = 0
increasesThree [_,_,_] = 0
increasesThree all@(w:_:_:z:xs)
    | w < z = 1 + increasesThree(tail all)
    | otherwise = increasesThree(tail all)
