import Data.Set (fromList, toList)
import Data.List (nub)
import Text.Parsec

main = do
    txt <- readFile "day13.txt"
    let (Right (cords, instructions)) = parse parser "" txt
    let part1 = folds [(head instructions)] cords
    print (length (nub part1))
    let part2 = folds instructions cords
    let (xBound, yBound) = getBounds part2 (0,0)
    putStr (draw part2 (xBound+1, yBound+1) (0,0))

folds :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
folds [] paper = paper
folds ((dir, x):xs) paper = case dir of
    'y' -> folds xs (foldPaper paper (0, x))
    'x' -> folds xs (foldPaper paper (x, 0))

foldPaper :: [(Int, Int)] -> (Int,Int) -> [(Int,Int)]
foldPaper [] _ = []
foldPaper ((x,y):xs) (i,j)
    | i == 0 = (if y > j then (x, (j*2) - y) else (x, y)) : foldPaper xs (i,j)
    | j == 0 = (if x > i then ((i*2) - x, y) else (x, y)) : foldPaper xs (i,j)
    | otherwise = [] -- always only one fold

draw :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> [Char]
draw points (i,j) (x,y)
    | j == y = []
    | i == x = '\n' : draw points (i,j) (0,y+1)
    | otherwise = (if (checkPoints points (x,y)) then '█' else ' ') : draw points (i,j) (x+1,y)

checkPoints :: [(Int,Int)] -> (Int,Int) -> Bool
checkPoints [] _ = False
checkPoints ((x,y):xs) (i,j) = if (x,y) == (i,j) then True else checkPoints xs (i,j)

getBounds :: [(Int,Int)] -> (Int, Int) -> (Int, Int)
getBounds [] (i,j) = (i,j)
getBounds ((x,y):xs) (i,j) = getBounds xs (max i x, max j y)

-- I looked at https://github.com/DestyNova/advent_of_code_2021/blob/main/day13/Part1.hs to help understand Parsec - I need to do more research to better my understanding here
parser :: Parsec String () ([(Int, Int)], [(Char, Int)])
parser = do
    cords <- do {[x,y] <- number `sepBy1` char ','; return (x,y)} `sepEndBy1` newline
    newline -- data/instructions are separated by a newline
    instructions <- foldInstruction `sepEndBy1` newline
    return (cords, instructions)
-- this is way better than the original solution
foldInstruction = do
    string "fold along "
    dir <- char 'x' <|> char 'y'
    char '='
    v <- number
    return (dir, v)

number :: Parsec String () Int
number = read <$> many1 digit
