main = do
    txt <- readFile "day13.txt"
    let cords = map (splitl (==',')) (lines txt)
    print cords


foldPaper :: [(Int, Int)] -> (Int,Int) -> [(Int,Int)]
foldPaper ((x,y):xs) (i,j)
    | i == 0 = if y > j then (x, y - j) else (x, y)
    | j == 0 = if x > i then (x - i, y) else (x, y)
    | otherwise = [] -- always only one fold


splitl :: (Char -> Bool) -> String -> [String]
splitl p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitl p s''
                            where (w, s'') = break p s'
