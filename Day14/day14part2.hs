import Text.Parsec
import Data.Ord
import Data.List

import qualified Data.Map as Map
import Data.Map (Map)

type Rules = [(Char, Char, Char)]
type Pairs = Map (Char, Char) Int
type CountChar = Map Char Int

main = do
    txt <- readFile "day14.txt"
    let Right((template, rules)) = parse parser "" txt
    let list = Map.fromListWith (+) (splitWord template)
    let freqChar = Map.fromListWith (+) (map (\n -> (n,1)) template)
    let part1Base = step 10 (list, freqChar) rules
    let part1 = sortBy (comparing snd) (Map.toList part1Base)
    print (snd(last part1) - snd(head part1) )
    let part2Base = step 40 (list, freqChar) rules
    let part2 = sortBy (comparing snd) (Map.toList part2Base)
    print part2
    print (snd(last part2) - snd(head part2) )

-- correct
splitWord :: String -> [((Char, Char), Int)]
splitWord [] = []
splitWord [i] = []
splitWord (i:j:xs) = ((i, j), 1) : splitWord (j:xs)

step :: Int -> (Pairs, CountChar) -> Rules -> CountChar
step 0 (pairMap, freqChar) rules = freqChar
step i (pairMap, freqChar) rules = step (i-1) (adjustVals pairMap freqChar rules) rules

adjustVals :: Pairs -> CountChar -> Rules -> (Pairs, CountChar)
adjustVals pairMap freqChar [] = (pairMap, freqChar)
adjustVals pairMap freqChar ((i,j,c):xs) = (Map.unionWith (+) thisPair nextPair, Map.insertWith (+) c matches nextCount)
    where
        matches = Map.findWithDefault 0 (i,j) pairMap
        newMap = if matches > 0 then Map.delete (i,j) pairMap else pairMap
        thisPair = if matches > 0 then Map.fromList [((i,c), matches), ((c,j), matches)] else Map.empty
        (nextPair, nextCount) = adjustVals newMap freqChar xs

parser :: Parsec String () (String, Rules)
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
