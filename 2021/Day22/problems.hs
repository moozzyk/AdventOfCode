import System.IO

data Operation = On | Off deriving (Show, Enum, Eq)

data Cuboid = Cuboid { rangeX :: (Int, Int)
                     , rangeY :: (Int, Int)
                     , rangeZ :: (Int, Int)
                     , action :: Operation
                     } deriving (Show)

main = do
    raw <- readFile "input.txt"
    let cuboids = parseInput $ lines raw
    print $ problem1 $ take 20 cuboids

problem1 cuboids =
    let
        coords = [(x,y,z) | x <- [-50..50]
                          , y <- [-50..50]
                          , z <- [-50..50]]
    in
        length $ filter (\s -> s == On) $ map (\c -> calculateState c cuboids) coords

calculateState coord cuboids =
    foldl (\s c -> calculateNewState coord c s) Off cuboids

calculateNewState coord cuboid state
    | inRange coord cuboid = action cuboid
    | otherwise            = state

inRange :: (Int, Int, Int) -> Cuboid -> Bool
inRange (x, y, z) cuboid =
    let (Cuboid (x1, x2) (y1, y2) (z1, z2) _) = cuboid
    in x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2

parseInput :: [String] -> [Cuboid]
parseInput ls = map parseLine ls

parseLine :: String -> Cuboid
parseLine l =
    let
        w = words l
        action = if (head w) == "on" then On else Off
        (x:y:z:_) = map parseRange $ map (drop 2) $ words $ map (\c -> if c == ',' then ' ' else c) $ last w
    in
        Cuboid x y z action

parseRange :: String -> (Int, Int)
parseRange range =
    let r = words $ map (\c -> if c == '.' then ' ' else c) $ range
    in (read (head r)::Int, read (last r)::Int)