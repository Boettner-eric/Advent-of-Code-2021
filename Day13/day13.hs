import Data.Set (fromList, toList)
import Data.List (nub)
import System.IO

main = do
    txt <- readFile "day13.txt"
    let cords = map (convertCoords) (lines txt)
    let part1 = folds [('x', 655)] cords
    print (length (nub part1))
    -- I need to automate / learn parsing better in haskell
    let foldList = [('x',655), ('y',447), ('x',327), ('y',223), ('x',163),('y',111), ('x',81), ('y',55), ('x',40), ('y',27), ('y',13), ('y',6)]
    let part2 = folds foldList cords
    let (xBound, yBound) = getBounds part2 (0,0)
    putStr (draw part2 (xBound+1, yBound+1) (0,0))

folds :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
folds [] paper = paper
folds ((dir, x):xs) paper = case dir of
    'y' -> folds xs (foldPaper paper (0, x))
    'x' -> folds xs (foldPaper paper (x, 0))

draw :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> [Char]
draw points (i,j) (x,y)
    | j == y = []
    | i == x = '\n' : draw points (i,j) (0,y+1)
    | otherwise = (if (checkPoints points (x,y)) then '#' else ' ') : draw points (i,j) (x+1,y)

checkPoints :: [(Int,Int)] -> (Int,Int) -> Bool
checkPoints [] _ = False
checkPoints ((x,y):xs) (i,j) = if (x,y) == (i,j) then True else checkPoints xs (i,j)

getBounds :: [(Int,Int)] -> (Int, Int) -> (Int, Int)
getBounds [] (i,j) = (i,j)
getBounds ((x,y):xs) (i,j) = getBounds xs (max i x, max j y)

convertCoords :: String -> (Int, Int)
convertCoords a = (head parsed, last parsed)
    where
        parsed = map read (splitl (==',') a) :: [Int]

foldPaper :: [(Int, Int)] -> (Int,Int) -> [(Int,Int)]
foldPaper [] _ = []
foldPaper ((x,y):xs) (i,j)
    | i == 0 = (if y > j then (x, (j*2) - y) else (x, y)) : foldPaper xs (i,j)
    | j == 0 = (if x > i then ((i*2) - x, y) else (x, y)) : foldPaper xs (i,j)
    | otherwise = [] -- always only one fold

splitl :: (Char -> Bool) -> String -> [String]
splitl p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitl p s''
                            where (w, s'') = break p s'
