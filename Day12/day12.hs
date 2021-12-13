import Data.Char
import Data.Set (toList, fromList)
import Data.List (nub, isInfixOf)

main = do
    txt <- readFile "day12.txt"
    let input = map (splitl (=='-')) (lines txt)
    let part1 = findPath input input "start" ["start"] 1
    print (length part1)
    let part2 = findPath input input "start" ["start"] 2
    print (length part2)

findPath :: [[String]] -> [[String]] -> String -> [String] -> Int -> [[String]]
findPath _ [] _ current _ = []
findPath graph ([x,y]:xs) start current allowed
    | start == "end" = [current]
    | start /= "start" && "start" `elem` [x,y] = next
    | twice = next
    | x == start = findPath graph graph y (current ++ [y]) allowed ++ next
    | y == start = findPath graph graph x (current ++ [x]) allowed ++ next
    | otherwise = next
        where
            smallCaves = filter allLower current
            twice = length smallCaves > length (nub smallCaves) + allowed -1
            next = findPath graph xs start current allowed

allLower i = all (==True) (map isLower i)

splitl :: (Char -> Bool) -> String -> [String]
splitl p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitl p s''
                            where (w, s'') = break p s'
