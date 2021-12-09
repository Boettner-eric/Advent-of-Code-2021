import qualified Data.Map as Map
import Data.List (filter, findIndex)
import Data.Maybe (fromMaybe)

main = do
    txt <- readFile "day8.txt"
    let part1 = limitedCount (fmap (splitl (=='|')) (lines txt))
    let part2 = countAll (lines txt)
    print (part1)
    print (sum part2)

--  0 0 0
-- 1      3
-- 1      3
-- 1      3
--  2 2 2
-- 4      6
-- 4      6
-- 4      6
--  5 5 5

-- 1: [3,6] 2
-- 2: [0,3,2,4,5] 5
-- 3: [0,3,2,6,5] 5
-- 4: [1,2,3,6] 4
-- 5: [0,1,2,6,5] 5
-- 6: [0,1,2,4,5,6] 6
-- 7: [0,3,6] 3
-- 8: [1..6] 7
-- 9: [0,1,2,3,5,6] 6
-- 0: [0,1,3,4,5,6] 6

-- check if x fits inside of y (i.e. 1: [3,6] fits inside 7: [0,3,6] etc.)
shareLines :: String -> String -> Bool
shareLines x y = (not (elem False (fmap (\i -> elem i y) x)))

-- utility function to check if two strings are the same
sameString :: String -> String -> Bool
sameString x y = (length y == length x) && all (==True) (fmap (\i -> elem i x) y)

-- remove duplicate strings from data (abcd == dcab)
noDupes :: [String] -> [String]
noDupes [] = []
noDupes (x:xs) = x : noDupes (filter (\i -> not (sameString x i)) xs)

-- given an array of strings figure out each mapping (by relation)
decode :: [String] -> [String]
decode [] = []
decode list = [zero, one, two, three, four, five, six, seven, eight, nine]
    where
        lengths = Map.fromListWith (++) (fmap (\i -> (length i, [i])) list)
        -- unique lengths (given)
        one = concat (lengths Map.! 2)
        seven = concat (lengths Map.! 3)
        four = concat (lengths Map.! 4)
        eight = concat (lengths Map.! 7)
        -- length 6
        six = concat (filter (\i -> not (shareLines one i)) (lengths Map.! 6)) -- only one of length 6 w/o overlap w/ one
        zero = concat (filter (\i -> (shareLines one i) && not (shareLines four i)) (lengths Map.! 6)) -- overlaps one but not four
        nine = concat (filter (\i -> (shareLines one i) && (shareLines four i)) (lengths Map.! 6)) -- overlaps one and four
        -- length 5
        three = concat (filter (\i -> (not (shareLines four i) && (shareLines one i))) (lengths Map.! 5)) -- overlaps one but not six
        five = concat (filter (\i -> shareLines i six) (lengths Map.! 5)) -- six overlaps 5 perfectly
        two = concat (filter (\i -> not (shareLines i six) && not (shareLines one i)) (lengths Map.! 5)) -- doesn't overlap one or six


limitedCount :: [[String]] -> Int
limitedCount [] = 0
limitedCount (x:xs) = length (filter (\n -> elem n [2,3,4,7]) (fmap length (words (last x)))) + (limitedCount xs)

countAll :: [String] -> [Int]
countAll [] = []
countAll (x:xs) = p : countAll xs
    where
        [i, j] = splitl (=='|') x
        mapping = decode (noDupes (words (i ++ j)))
        p = convertNum 1000 (fmap (\k -> lookupScramble 0 k mapping) (words j))

lookupScramble :: Int -> String -> [String] -> Int
lookupScramble _ _ [] = -1
lookupScramble i k (x:xs) = if (sameString k x) then i else lookupScramble (i+1) k xs

convertNum :: Int -> [Int] -> Int
convertNum _ [] = 0
convertNum i (x:xs) = (x * i) + convertNum (i `div` 10) xs

splitl :: (Char -> Bool) -> String -> [String]
splitl p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitl p s''
                            where (w, s'') = break p s'
