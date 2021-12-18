import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

main = do
    txt <- readFile "day15.txt"
    let x = map (map (\i -> read (i:[]))) (lines txt) :: [[Int]]
    let big = grow (bigify x) 4
    print (setup x)
    print (setup big)

type Point = ((Int, Int), Int)
type PointMap = Map (Int, Int) Int

setup :: [[Int]] -> Int
setup graph = dijkstra costMap' calcMap'
    where
        (costMap, initMap) = transformData 0 graph
        costMap' = Map.fromList costMap
        calcMap  = Map.fromList initMap
        calcMap' = Map.insert (0,0) 0 calcMap

dijkstra :: PointMap -> PointMap -> Int
dijkstra costMap calcMap
    | (i,j) == fst (Map.findMax costMap) = k -- destination node is visited
    | otherwise = dijkstra costMap newMap'
        where
            ((i,j), k) = minimumBy (comparing snd) (Map.toList calcMap) -- pop off minimum
            newMap = updatePoints k [(i-1,j),(i+1,j),(i,j-1),(i,j+1)] costMap calcMap -- add four points
            newMap' = Map.delete (i,j) newMap -- remove this point (just visited)

updatePoints :: Int -> [(Int, Int)] -> PointMap -> PointMap -> PointMap
updatePoints _ [] _ calcMap = calcMap
updatePoints cost ((i,j):xs) costMap calcMap
    | inBounds (i,j) costMap = updatePoints cost xs costMap newMap
    | otherwise = updatePoints cost xs costMap newMap
    where
        calcCost = lookupWith (i,j) costMap + cost
        currentCost = lookupWith (i,j) calcMap
        newMap = if (calcCost < currentCost) then (Map.adjust (\_ -> calcCost) (i,j) calcMap) else calcMap

inBounds :: (Int, Int) -> PointMap -> Bool
inBounds (i,j) costMap
    | i > (fst $ fst (Map.findMax costMap)) || j > (snd $ fst (Map.findMax costMap)) = False
    | i < 0 || j < 0 = False
    | otherwise = True

lookupWith :: (Int, Int) -> PointMap -> Int
lookupWith (i,j) costMap = case (Map.lookup (i,j) costMap) of
    Just a -> a
    Nothing -> 1000000

transformData :: Int -> [[Int]] -> ([Point], [Point])
transformData _ [] = ([],[])
transformData y (k:ks) = (valRow ++ i, zeroRow ++ j)
    where
        valRow = zipWith (\x i -> ((y, x), i)) [0..] k :: [Point]
        zeroRow = zipWith (\x i -> ((y, x), 10000)) [0..] k :: [Point]
        (i,j) = transformData (y+1) ks


bigify :: [[Int]] -> [[Int]]
bigify [] = []
bigify (x:xs) = (x ++ (fn x 1) ++ (fn x 2) ++ (fn x 3) ++ (fn x 4)) : bigify xs

bigify2 :: [[Int]] ->  [[Int]]
bigify2 [] = []
bigify2 (x:xs) = (map (\i -> (i `mod` 9) + 1) x) : (bigify2 xs)

grow :: [[Int]] -> Int -> [[Int]]
grow x 0 = x
grow x i = x ++ grow (bigify2 x) (i-1)

fn :: [Int] -> Int -> [Int]
fn x i = (zipWith (\i j -> ((i+j-1) `mod` 9) + 1) x (replicate (length x) i))
