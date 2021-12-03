module Main where

main = do
    txt <- readFile "day2.txt"
    let commands = convertData (words <$> lines txt)
    let part1 = travelDistance commands
    print (head part1 * last part1)
    let part2 = aimTravel 0 commands
    print (head part2 * last part2)

-- function that takes commands and outputs depth and horizontal travel
travelDistance :: [(String, Int)] -> [Int]
travelDistance [] = [0,0]
travelDistance ((command, unit):xs) = case command of
                                        "forward" -> zipWith (+) (travelDistance xs) [unit, 0]
                                        "up" -> zipWith (+) (travelDistance xs) [0, (-1) * unit]
                                        "down" -> zipWith (+) (travelDistance xs) [0, unit]

-- function that takes commands and outputs aim, depth and horizontal travel
aimTravel :: Int -> [(String, Int)] -> [Int]
aimTravel _ [] = [0,0]
aimTravel i ((command, unit):xs) = case command of
                                    "forward" -> zipWith (+) (aimTravel i xs) [i * unit, unit]
                                    "up" -> aimTravel (i - unit) xs
                                    "down" -> aimTravel (i + unit) xs

-- helper function to change data from two dim String to array of tuple (String, Int)
convertData :: [[String]] -> [(String, Int)]
convertData [] = []
convertData (x:xs) = [(head x, read (last x))] ++ convertData xs
