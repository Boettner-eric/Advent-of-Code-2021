import Text.Parsec
import Data.Ord
import Data.List

import qualified Data.Map as Map
import Data.Map (Map)
-- idea for part2
{-
    Make a map of each pair (Char,Char) -> Int
    for each step:
        if a pair is destroyed decrement its counter and add to counters for
            (a,b) (a,b,c) -> (a,c)+1, (c,b)+1, (a,b)-1
        if a new pair is found add a mapping for it

-}
type Rules = [(Char, Char, Char)]
type Pairs = Map (Char, Char) Int
type CountChar = Map Char Int

main = do
    txt <- readFile "sample.txt"
    let Right((template, rules)) = parse parser "" txt
    let list = Map.fromList (splitWord template)
    print list
    let freqChar = Map.fromListWith (+) (map (\n -> (n,1)) template)
    let finalFreq = step 40 (list, freqChar) rules
    let part2 = sortBy (comparing snd) (Map.toList finalFreq)
    print finalFreq
    print part2
    print (snd(last part2) - snd(head part2) )

splitWord :: String -> [((Char, Char), Int)]
splitWord [] = []
splitWord (i:[]) = []
splitWord (i:j:xs) = ((i, j), 1) : splitWord (j:xs)

step :: Int -> (Pairs, CountChar) -> Rules -> CountChar
step 0 (pairMap, freqChar) rules = freqChar
step i (pairMap, freqChar) rules = step (i-1) (adjustVals pairMap freqChar rules) rules

adjustVals :: Pairs -> CountChar -> Rules -> (Pairs, CountChar)
adjustVals pairMap freqChar [] = (pairMap, freqChar)
adjustVals pairMap freqChar ((i,j,c):xs) = (Map.unionWith (+) thisPair nextPair, Map.insertWith (+) c matches nextCount)
    where
        matches = Map.findWithDefault 0 (i,j) pairMap
        thisPair = Map.fromList [((i,c), matches), ((c,j), matches)]
        (nextPair, nextCount) = adjustVals (Map.delete (i,j) pairMap) freqChar xs

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
