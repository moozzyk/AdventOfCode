import System.IO
import Data.Maybe
import Debug.Trace

data Operation = On | Off deriving (Show, Enum, Eq)

data Cuboid = Cuboid { rangeX :: (Int, Int)
                     , rangeY :: (Int, Int)
                     , rangeZ :: (Int, Int)
                     , action :: Operation
                     } deriving (Show)

main = do
    raw <- readFile "input.txt"
    let cuboids = parseInput $ lines raw
    print $ problem1 cuboids
    print $ problem2 cuboids

problem1slow :: [Cuboid] -> Int
problem1slow cuboids =
    let
        coords = [(x,y,z) | x <- [-50..50]
                          , y <- [-50..50]
                          , z <- [-50..50]]
    in
        length $ filter (\s -> s == On) $ map (\c -> calculateState c cuboids) coords

calculateState :: (Int, Int, Int) -> [Cuboid] -> Operation
calculateState coord cuboids =
    foldl (\s c -> calculateNewState coord c s) Off cuboids

calculateNewState :: (Int, Int, Int) -> Cuboid -> Operation -> Operation
calculateNewState coord cuboid state
    | inRange coord cuboid = action cuboid
    | otherwise            = state

inRange :: (Int, Int, Int) -> Cuboid -> Bool
inRange (x, y, z) cuboid =
    let (Cuboid (x1, x2) (y1, y2) (z1, z2) _) = cuboid
    in x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2


problem1 :: [Cuboid] -> Int
problem1 cuboids = calculateVolume $ filter (initializationCandidate) cuboids

initializationCandidate :: Cuboid -> Bool
initializationCandidate c =
    let
        (Cuboid (x1, x2) (y1, y2) (z1, z2) _) = c
    in
        x1 >= -50 && x2 <= 50 && y1 >= -50 && y2 <= 50 && z1 >= -50 && z2 <= 50

problem2 :: [Cuboid] -> Int
problem2 cuboids = calculateVolume cuboids

calculateVolume :: [Cuboid] -> Int
calculateVolume cuboids =
    let
        finalCuboids = foldl(\res c -> insert c res) [] cuboids
    in
        sum $ map (volume) finalCuboids

insert :: Cuboid -> [Cuboid] -> [Cuboid]
insert c cuboids =
    let
        newCuboids = catMaybes $ map (\cuboid -> if disjoint cuboid c then Nothing else Just $ intersect cuboid c) cuboids
    in
        if action c == On then
            cuboids ++ (c:newCuboids)
        else
            cuboids ++ newCuboids

volume :: Cuboid -> Int
volume c =
    let
        (Cuboid (x1, x2) (y1, y2) (z1, z2) action) = c
        vol = (1 + x2 - x1) * (1 + y2 - y1) * (1 + z2 - z1)
    in
        if action == On then vol else -vol

intersect :: Cuboid -> Cuboid -> Cuboid
intersect c1 c2 =
    let
        (Cuboid (c1x1, c1x2) (c1y1, c1y2) (c1z1, c1z2) c1Action) = c1
        (Cuboid (c2x1, c2x2) (c2y1, c2y2) (c2z1, c2z2) c2Action) = c2
        rangeX = (if c1x1 < c2x1 then c2x1 else c1x1, if c1x2 > c2x2 then c2x2 else c1x2)
        rangeY = (if c1y1 < c2y1 then c2y1 else c1y1, if c1y2 > c2y2 then c2y2 else c1y2)
        rangeZ = (if c1z1 < c2z1 then c2z1 else c1z1, if c1z2 > c2z2 then c2z2 else c1z2)
        action = if c1Action == On then Off else On
    in
        Cuboid rangeX rangeY rangeZ action

disjoint :: Cuboid -> Cuboid -> Bool
disjoint c1 c2 =
    let
        (Cuboid (c1x1, c1x2) (c1y1, c1y2) (c1z1, c1z2) _) = c1
        (Cuboid (c2x1, c2x2) (c2y1, c2y2) (c2z1, c2z2) _) = c2
    in
        c1x2 < c2x1 || c1x1 > c2x2 ||
        c1y2 < c2y1 || c1y1 > c2y2 ||
        c1z2 < c2z1 || c1z1 > c2z2;

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