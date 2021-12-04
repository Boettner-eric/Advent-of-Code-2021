import Data.Char (digitToInt)
import Data.List (foldl')

main = do
    txt <- readFile "day3.txt"
    let dataset = lines txt
    let epsilon = toDec (part1 0 dataset True)
    let gamma = toDec (part1 0 dataset False)
    print (epsilon * gamma)
    let ox = toDec (part2 0 dataset True)
    let co2 = toDec (part2 0 dataset False)
    print (ox * co2)

-- get most common/least common digit in each position
part1 :: Int -> [String] -> Bool -> String
part1 _ [] _ = []
part1 i list op
    | i < (length (head list)) = [compColumn (0,0) i list op] ++ part1 (i+1) list op
    | otherwise = ""

-- filter all data with char at index n
filterFn :: Int -> Char -> [String] -> [String]
filterFn _ _ [] = []
filterFn n cond (x:xs) = if (x !! n == cond) then [x] ++ filterFn n cond xs else filterFn n cond xs

-- call filter Fn until there is only two/one num left
part2 :: Int -> [String] -> Bool -> String
part2 _ [x] op = x -- correct value
part2 n list op = part2 (n+1) (filterFn n (compColumn (0,0) n list op) list) op

-- compare the number of ones and zeros in eac column
compColumn :: (Int, Int) -> Int -> [String] -> Bool -> Char
compColumn (ones, zeros) n [] op = if ((ones < zeros) /= op) then '0' else '1' -- XOR with operator
compColumn (ones, zeros) n (x:xs) op = if (x !! n) == '1'
    then compColumn (ones+1, zeros) n xs op
    else compColumn (ones, zeros+1) n xs op

-- from internet
toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
