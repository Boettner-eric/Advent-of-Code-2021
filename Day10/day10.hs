import Data.List (sort)

main = do
    txt <- readFile "day10.txt"
    let line = lines txt
    let part1 = map (\i -> checkLine i []) line
    print (sumLine part1)
    let incomplete = map (\i -> completeLine (checkLine i []) 0) line
    let part2 = sort (filter (\i -> i /= 0) incomplete)
    print (part2 !! (div (length part2) 2))

sumLine :: [String] -> Int
sumLine [] = 0
sumLine (x:xs)
    | x == ")" = 3 + sumLine xs
    | x == "]" = 57 + sumLine xs
    | x == "}" = 1197 + sumLine xs
    | x == ">" = 25137 + sumLine xs
    | otherwise = sumLine xs

checkLine :: String -> String -> String
checkLine [] history = history
checkLine (x:xs) history
    | x `elem` ['[','{','<','('] = checkLine xs (x : (history))
    | x == ')' = if (head history) == '(' then checkLine xs (tail history) else ")"
    | x == ']' = if (head history) == '[' then checkLine xs (tail history) else "]"
    | x == '}' = if (head history) == '{' then checkLine xs (tail history) else "}"
    | x == '>' = if (head history) == '<' then checkLine xs (tail history) else ">"
    | otherwise = "error"

completeLine :: String -> Int -> Int
completeLine [] score = score
completeLine (x:xs) score
    | x == '(' = completeLine xs (score * 5 + 1)
    | x == '[' = completeLine xs (score * 5 + 2)
    | x == '{' = completeLine xs (score * 5 + 3)
    | x == '<' = completeLine xs (score * 5 + 4)
    | otherwise = 0
