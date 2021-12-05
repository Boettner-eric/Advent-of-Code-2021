import Data.List (findIndices, transpose)

main = do
    txt <- readFile "day4.txt"
    let dataset = lines txt
    let results = map read (split (head dataset)) :: [Int] -- list of bingo numbers
    let boards = zipBoard (makeBoard (tail dataset)) -- list of boards

    let (value, winner) = part1 results boards
    let (lastValue, lastWinner) = part2 results boards [] []

    print ((calculateScore winner) * value)
    print ((calculateScore lastWinner) * lastValue)

-- returns board
part1 :: [Int] -> [[Int]] -> (Int, [Int])
part1 [] _ = (0, []) -- all bingo results parsed w/ no winner
part1 (x:xs) boards = if bingo /= []
    then ( x, bingo)
    else part1 xs updated
    where
        updated = markBoards x boards -- mark all the boards w/ X
        bingo = findWinner updated -- search all boards for bingo

-- results, boards, winning boards, winning numbers
part2 :: [Int] -> [[Int]] -> [[Int]] -> [Int] -> (Int, [Int])
part2 [] _ winners results = (last results, last winners)
part2 (x:xs) boards winners results = if bingos /= []
    then part2 xs filtered (winners ++ [bingos]) (results ++ [x])
    else part2 xs updated winners results
    where
        updated = markBoards x boards -- mark all the boards w/ X
        (filtered, bingos) = filterWinners updated updated [] -- search all boards for bingo
        -- filtered = filter (\n -> n `elem` bingos) updated) working on better filtering

-- can't use sum bc of -1s that mark board
calculateScore :: [Int] -> Int
calculateScore [] = 0
calculateScore (x:xs)
    | x /= -1 = x + calculateScore xs
    | otherwise = calculateScore xs

-- I dislike the way I wrote this but it works
filterWinners :: [[Int]] -> [[Int]] -> [Int] -> ([[Int]], [Int])
filterWinners i [] winners = (i, winners)
filterWinners i (x:xs) winners = if (check 0 x)
    then filterWinners (filter (\n -> n /= x) i) xs (winners ++ x)
    else filterWinners i xs winners

-- search boards for first winning board and return it
findWinner :: [[Int]] -> [Int]
findWinner [] = []
findWinner (x:xs) = if (check 0 x) then x else findWinner xs

check :: Int -> [Int] -> Bool
check 5 _ = False
check x list = if (horizontal || vertical) then True else check (x+1) list
    where
        row = map (\i -> i + (x * 5)) [0,1,2,3,4]
        column = map (\i -> i + x) [0,5,10,15,20]
        horizontal = all (==(-1)) (map (list !!) row)
        vertical = all (==(-1)) (map (list !!) column)

zipBoard :: [[Int]] -> [[Int]]
zipBoard [] = []
zipBoard (a:b:c:d:e:xs) = [a ++ b ++ c ++ d ++ e] ++ zipBoard xs

-- need to alter to make board 5x5 instead of 5x1
makeBoard :: [String] -> [[Int]]
makeBoard [] = []
makeBoard (x:xs)
    | x == "" = makeBoard xs
    | otherwise = [map read (words x) :: [Int]] ++ makeBoard xs

-- mark each instance of num on boards
markBoards :: Int -> [[Int]] -> [[Int]]
markBoards _ [] = []
markBoards i (x:xs) = [mark y x] ++ (markBoards i xs)
    where
        y = findIndices (==i) x

-- change value from current to -1
mark :: [Int] -> [Int] -> [Int]
mark [] list = list
mark (x:xs) list = result
    where
        (i,_:j) = splitAt x list -- _:j -> _ takes the element out
        result = i ++ -1 : j -- rejoin with -1 marker

split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]
