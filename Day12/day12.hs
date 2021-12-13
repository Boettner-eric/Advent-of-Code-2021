import Data.Char
import Data.Set (toList, fromList)
import Data.List

main = do
    txt <- readFile "sample.txt"
    let input = map (splitl (=='-')) (lines txt)
    let y = findPath input input "start" "start" False
    print (length y)
    let x = findPath input input "start" "start" True
    print (length x)
    let results = toList (fromList (x))
    print (length results)

findPath :: [[String]] -> [[String]] -> String -> String -> Bool -> [String]
findPath _ [] _ current _ = []
findPath graph (x:xs) start current twice
    | start == "end" = [current]
    | start /= "start" && "start" `elem` x = next
    | ((head x) ++ ",") `isInfixOf` current && allLower (head x) && twice = next
    | ((last x) ++ ",") `isInfixOf` current && allLower (last x) && twice = next
    | head x == start = findPath graph graph (last x) (current ++',': last x) (i || j || twice) ++ next
    | last x == start = findPath graph graph (head x) (current ++',': head x) (i || j || twice) ++ next
    | otherwise = next
        where
            i = ((head x) ++ ",") `isInfixOf` current && allLower (head x)
            j = ((last x) ++ ",") `isInfixOf` current && allLower (last x)
            next = findPath graph xs start current (i || j || twice)

allLower i = all (==True) (map isLower i)

splitl :: (Char -> Bool) -> String -> [String]
splitl p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitl p s''
                            where (w, s'') = break p s'
