import Data.List (sort)

main = do
    txt <- readFile "day11.txt"
    let input = formatInput (lines txt)
    print (steps input 0 100)
    print (allFlash input)

formatInput :: [String] -> [[Int]]
formatInput [] = []
formatInput (x:xs) = map (\i -> read (i:[]) :: Int) x : formatInput xs

steps :: [[Int]] -> Int -> Int -> ([[Int]], Int)
steps points flashes 0 = (points, flashes)
steps points flashes i = steps newPoints (flashes + newFlashes) (i-1)
    where
        (newPoints, newFlashes) = step points


allFlash :: [[Int]] -> Int
allFlash points = if all (==True) (map (all (==0)) newPoints) then 1 else 1 + allFlash newPoints
    where
        (newPoints, _) = step points


step :: [[Int]] -> ([[Int]], Int)
step [] = ([],0)
step list = (settleDown updatedpoints, flashes)
    where
        addlist = addOne list
        startingPoints = search addlist (0,0) -- list of points > 9  [(Int, Int)]
        (flashes, updatedpoints) = propagate addlist startingPoints 0

--First, the energy level of each octopus increases by 1.
addOne :: [[Int]] -> [[Int]]
addOne [] = []
addOne (x:xs) = map (+1) x : addOne xs

--Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.
settleDown :: [[Int]] -> [[Int]]
settleDown [] = []
settleDown (x:xs) = map (\i -> if i > 9 then 0 else i) x : settleDown xs

-- Update values for each point in an array of points
updatePoints :: [[Int]] -> [(Int,Int)] -> [[Int]]
updatePoints points [] = points
updatePoints points ((i,j):xs) -- omit invalid indices
    | i < 0 || j < 0 || j >= length points || i >= length (points !! 0) = updatePoints points xs
    | otherwise = updatePoints (beginY ++ endY') xs
        where
            (beginY, endY) = splitAt j points
            (beginX, _:endX) = splitAt i (endY !! 0)
            endY' = (beginX ++ ((points !! j !! i) + 1):endX) : (tail endY)

lookupSafe :: [[Int]] -> (Int, Int) -> Int
lookupSafe [] _ = 0
lookupSafe points (i,j)
    | i < 0 || j < 0 || j >= length points || i >= length (points !! 0) = 0
    | otherwise = points !! j !! i

-- get all points > 9
search :: [[Int]] -> (Int, Int) -> [(Int, Int)]
search points (i,j)
    | j >= length points = []
    | i >= length (points !! 0) = search points (0,j+1)
    | (points !! j !! i) > 9 = (i,j) : search points (i+1,j)
    | otherwise = search points (i+1, j)

--           graph       queue          flashes # of flashes and point graph
propagate :: [[Int]] -> [(Int, Int)] -> Int -> (Int, [[Int]])
propagate points [] flashes = (flashes, points)
propagate points ((i,j):queue) flashes
    | i < 0 || j < 0 || j >= length points || i >= length (points !! 0) = propagate points queue flashes
    | otherwise = propagate newPoints updatedQueue (flashes + 1)
        where
            neighbors = [(i-1,j-1),(i-1,j),(i-1,j+1),(i+1,j-1),(i+1,j),(i+1,j+1),(i,j-1),(i,j+1)]
            newPoints = updatePoints points neighbors
            updatedQueue = (filter (\n -> (lookupSafe newPoints n) == 10) neighbors) ++ queue
