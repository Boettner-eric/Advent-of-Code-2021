import Text.Parsec
import Data.Ord
import Data.List

import qualified Data.Map as Map

main = do
    txt <- readFile "day14.txt"
    let Right((template, rules)) = parse parser "" txt
    let part1 = (step 10 template rules)
    let maxVal = maximum $ map length . group $ sort part1
    let minVal = minimum $ map length . group $ sort part1
    print (maxVal)
    print (minVal)

-- split word then insert pairs then zip it back up for the next round
step :: Int -> String -> [(Char, Char, Char)] -> String
step 0 template rules = template
step i template rules = step (i-1) (zipWord (insertChars rules (splitWord template))) rules

insertChars :: [(Char, Char, Char)] -> [(Char, Char, Char)] -> [(Char, Char, Char)]
insertChars [] current = current
insertChars (x:xs) current = insertChars xs (insertChar x current)

insertChar :: (Char, Char, Char) -> [(Char, Char, Char)] -> [(Char, Char, Char)]
insertChar _ [] = []
insertChar (a,b,c) ((i,j,z):template)
    | a == i && j == b = (i, j, c) : insertChar (a,b,c) template
    | otherwise = (i, j, z) : insertChar (a,b,c) template

splitWord :: String -> [(Char, Char, Char)]
splitWord [] = []
splitWord (i:[]) = []
splitWord (i:j:xs) = (i, j, ' ') : splitWord (j:xs)

zipWord :: [(Char, Char, Char)] -> String
zipWord [] = ""
zipWord ((a,b,c):[]) = if c == ' ' then a : b : [] else a : c : b : [] -- if last add final char b
zipWord ((a,b,c):xs) = if c == ' ' then a : zipWord xs else a : c : zipWord xs -- ommit b for overlap

parser :: Parsec String () (String, [(Char, Char, Char)])
parser = do
    template <- (many1 letter)
    newline
    newline
    rules <- (do
        x <- letter
        y <- letter
        string " -> "
        z <- letter
        return (x,y,z)) `sepEndBy1` newline
    return (template, rules)
