import Data.Char
import Data.Bool(bool)

main = do
    txt <- readFile "day3.txt"
    let binary = splitBinary (lines txt)
    let epsilon = convertSum True <$> binary
    let gamma = convertSum False <$> binary
    print (bintodec epsilon * bintodec gamma)

-- take sum of digits and convert to bool
-- True -> GCB, False -> LCB
convertSum :: Bool -> Int -> Bool
convertSum True x = x > 500
convertSum _ x = x < 500

-- part 2
-- take convertSum and if True filter out starting in 0 else 1
-- if even then pick the one

-- convert string of digits to array of ints
binString :: String -> [Int]
binString [] = []
binString (x:xs) = [ord x - 48] ++ (binString xs)

-- add each power or digit together
splitBinary :: [String] -> [Int]
splitBinary [] = take 12 (repeat 0) -- fill empty array
splitBinary (x:xs) = zipWith (+) (binString x) (splitBinary xs)

-- had to look this one up due to lack of knowledge
--https://stackoverflow.com/questions/56663207/binary-to-decimal-in-haskell-without-using-recursion-or-list-comprehensions?rq=1
bintodec :: (Foldable f, Integral i) => f Bool -> i
bintodec = foldl (\a -> (+) (2*a) . bool 0 1) 0
