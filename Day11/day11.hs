import Data.List (sort)

main = do
    txt <- readFile "day9.txt"
    let input = formatInput (lines txt)
    let points = lowPoints (0,0) input
    let part1 = lookupPoints input points
    print (sum (part1) + length (part1)) -- add one for each basin
    let part2 = sort (map (\i -> findBasin input [] [i]) points); -- sort results by size
    print (product (drop ((length part2) - 3) part2)) -- product of top 3 basin sizes

formatInput :: [String] -> [[Int]]
formatInput [] = []
formatInput (x:xs) = map (\i -> read (i:[]) :: Int) x : formatInput xs


step :: [[Int]] -> [[Int]]
step [] = []
step list = addOne list

addOne :: [[Int]] -> [[Int]]
addOne [] = []
addOne (x:xs) = map (+1) x : step xs

--Then, any octopus with an energy level greater than 9 flashes. This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent. If this causes an octopus to have an energy level greater than 9, it also flashes. This process continues as long as new octopuses keep having their energy level increased beyond 9. (An octopus can only flash at most once per step.)


settleDown :: [[Int]] -> [[Int]]
settleDown [] = []
settleDown (x:xs) = map (\i -> if i > 9 then 0 else i) : settleDown xs


-- find values for each point in an array of points
updatePoints :: [[Int]] -> [(Int,Int)] -> [[Int]]
updatePoints _ [] = []
updatePoints points ((i,j):xs) -- omit invalid indices
    | i < 0 || j < 0 || j >= length points || i >= length (points !! 0) = updatePoints points xs
    | otherwise = updatePoints (beginning ++ end') xs
        where
            (beginning, end) = splitAt j points
            desired = head end
            desired' = i:desired
            end' = desired' : (tail end)

-- find all the low points (less than all neighbors)
lowPoints :: (Int, Int) -> [[Int]] -> [(Int,Int)]
lowPoints (i,j) points
    | i >= length (points !! 0) = lowPoints (0, j+1) points
    | j >= length points = []
    | otherwise = if (all (>(points !! j !! i)) (lookupPoints points [(i-1, j), (i+1,j), (i,j-1), (i,j+1)]))
        then (i,j) : lowPoints (i+1,j) points
        else lowPoints (i+1,j) points

--           graph      visited           queue         size of basin
findBasin :: [[Int]] -> [(Int, Int)] -> [(Int, Int)] -> Int
findBasin _ _ [] = 0
findBasin points visited ((i,j):queue)
    | i < 0 || j < 0 || j >= length points || i >= length (points !! 0) = findBasin points ((i,j) : visited) queue
    | (i,j) `elem` visited = findBasin points ((i,j) : visited) queue
    | (points !! j !! i) >= 9 = findBasin points ((i,j) : visited) queue
    | otherwise = 1 + findBasin points ((i,j) : visited) (queue ++ [(i-1,j),(i+1,j),(i,j-1),(i,j+1)])
