import qualified Data.Map as Map

main = do
    txt <- readFile "day6.txt"
    let dataset = map read (split (==',') txt) :: [Int]
    let fish = Map.fromListWith (+) (map (\i -> (i,1)) dataset) :: Fish
    print (sum (Map.elems (runDays 256 fish)))

type Fish = Map.Map Int Int

runDays :: Int -> Fish -> Fish
runDays 0 fish = fish
runDays i fish = runDays (i-1) ret
    where
        adjust (key, x) = if key == 0 then (8, x) else (key-1,x)
        newfish = Map.fromList (map adjust (Map.toAscList fish))
        spawn = Map.findWithDefault 0 0 fish -- count number of zeros
        ret = Map.insertWith (+) 6 spawn newfish -- add new fish to pool

split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'
