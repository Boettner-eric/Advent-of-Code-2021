main = do
    txt <- readFile "day3.txt"
    let binary = lines txt
    splitBinary binary

splitBinary :: [String] -> [[Ints]]
splitBinary [] -> [[]]
splitBinary (x:xs) -> zipWith (+) [binString x] splitBinary xs

binString :: String -> [Int]
binString [] -> []
binString (x:xs) -> [read x] ++ binString xs
