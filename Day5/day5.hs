import qualified Data.Map as Map

main = do
    txt <- readFile "day5.txt"
    let inputs = lines txt
    let finalMap = part1 inputs
    print (length (Map.filter (>1) finalMap))
    let finalMap2 = part2 inputs
    print (length (Map.filter (>1) finalMap2))

type CoordMap = Map.Map (Int, Int) Int

part1 :: [String] -> CoordMap
part1 [] = Map.empty
part1 (a:ab) = Map.unionWith (+) (part1 ab) updatedValues
    where
        (x,y) = convertCoords (head (words a))
        (w,z) = convertCoords (last (words a))
        updatedValues = Map.fromListWith (+) (getInBetween (x,y) (w,z))

part2 :: [String] -> CoordMap
part2 [] = Map.empty
part2 (a:ab) = Map.unionWith (+) (part2 ab) updatedValues
    where
        (x,y) = convertCoords (head (words a))
        (w,z) = convertCoords (last (words a))
        updatedValues = Map.fromListWith (+) (getAllBetween (x,y) (w,z))

convertCoords :: String -> (Int, Int)
convertCoords a = (head parsed, last parsed)
    where
        parsed = map read (split (==',') a) :: [Int]

-- find the start and end point of a line
mini = (\n -> (\l -> if l < n then (l,n) else (n,l)))

-- get all points between two points
getInBetween :: (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
getInBetween (x,y) (w,z)
    | x == w = (map (\n -> ((x,n), 1)) [a .. b])
    | y == z = (map (\n -> ((n,y), 1)) [c .. d])
    | otherwise = [] -- exclude diagonal lines
        where
            (a,b) = mini y z
            (c,d) = mini x w

getAllBetween :: (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
getAllBetween (x,y) (w,z)
    | x == w = (map (\n -> ((x,n), 1)) [a .. b])
    | y == z = (map (\n -> ((n,y), 1)) [c .. d])
    | otherwise = diagonal (x,y) (w,z)-- calculate diagonal lines here
        where
            (a,b) = mini y z
            (c,d) = mini x w

-- check every diagonal
diagonal :: (Int, Int) -> (Int, Int) -> [((Int, Int), Int)]
diagonal (x, y) (w, z)
    | x == w = [((w, z), 1)]
    | (x < w && y < z) = ((x, y), 1) : diagonal (x+1, y+1) (w, z) -- /
    | (x < w && y > z) = ((x, y), 1) : diagonal (x+1, y-1) (w, z) -- \
    | (x > w && y < z) = ((x, y), 1) : diagonal (x-1, y+1) (w, z) -- \
    | otherwise = ((x, y), 1) : diagonal (x-1, y-1) (w, z) -- /

split     :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'
