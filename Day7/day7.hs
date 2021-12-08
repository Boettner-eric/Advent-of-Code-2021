import Data.List (minimumBy)
import Data.Ord (comparing)

main = do
    txt <- readFile "day7.txt"
    let dataset = map read (split (==',') txt) :: [Int]
    let items = map (\n -> (countSteps n dataset)) [(minimum dataset) .. (maximum dataset)] -- test every number between min/max val
    print (minimumBy (comparing snd) items) -- find smallest fuel cost from pair (index, fuel cost)

-- I included the index with the fuel cost in case it was needed (it wasn't)
countSteps :: Int -> [Int] -> (Int, Int)
countSteps i list = (i, sum (map (\n -> div ( (abs (n-i)+1) * ( (abs (n-i))) ) 2) list)) -- cost of making that step for every value
-- 1 + ... + n = div ( (abs (n-i)+1) * ( (abs (n-i))) ) 2

split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'
