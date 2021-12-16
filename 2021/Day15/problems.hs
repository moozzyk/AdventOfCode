import System.IO
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let grid = M.fromList  $ parseInput $ lines raw
    print $ problem1 grid
    print $ problem2 grid

parseInput :: [String] -> [((Int, Int), Int)]
parseInput ls =
    let
        side = (length $ head ls) - 1
        coords = [(x, y) | y <- [0..side], x <- [0..side]]
    in  zip coords $ map digitToInt $ concat ls

problem1 :: M.Map (Int, Int) Int -> Int
problem1 grid =
    let
        side = maximum $ map snd $ M.keys grid
    in
        calculateRisk side side grid (S.fromList [(0, (0, 0))]) M.empty S.empty

problem2 :: M.Map (Int, Int) Int -> Int
problem2 grid =
    let
        side = maximum $ map snd $ M.keys grid
        maxSide = 5 * (side + 1) - 1
    in
        calculateRisk side maxSide grid (S.fromList [(0, (0, 0))]) M.empty S.empty

calculateRisk :: Int -> Int -> M.Map (Int, Int) Int -> S.Set (Int, (Int, Int)) -> M.Map (Int, Int) Int -> S.Set (Int, Int) -> Int
calculateRisk side maxSide grid candidates distances visited =
    let
        (dist, pos) = S.findMin candidates
        visited' = S.insert pos visited
        neighbors = unvistedNeighbors pos maxSide visited
        distNeigbhors = map (\n -> (dist + (safeLookup n grid (side + 1)), n)) neighbors
        candidates' = S.delete (dist, pos) $ foldr (\n c -> S.insert n c) candidates distNeigbhors
        distances' = updateDistances distNeigbhors distances
    in
        if pos == (maxSide, maxSide)
            then snd $ M.findMax distances
            else calculateRisk side maxSide grid candidates' distances' visited'

unvistedNeighbors :: (Int, Int) -> Int -> S.Set (Int, Int) -> [(Int, Int)]
unvistedNeighbors (x, y) side visited =
    filter (\c -> S.notMember c visited) $
        filter (\(a, b) -> a >= 0 && b >= 0 && a <= side && b <= side) $
            [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

updateDistances :: [(Int, (Int, Int))] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
updateDistances neighbors distances =
    foldr (\c d -> updateDistance c d) distances neighbors

updateDistance :: (Int, (Int, Int)) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
updateDistance (dist, pos) distances =
    let
        old = M.lookup pos distances
    in
        if old == Nothing || (fromJust old) > dist
            then M.insert pos dist distances
            else distances

safeLookup :: (Int, Int) -> M.Map (Int, Int) Int -> Int -> Int
safeLookup (x, y) grid side =
    let
        source = (x `mod` side, y `mod` side)
        baseValue = fromJust $ M.lookup source grid
        modifier = (x `div` side) + (y `div` side)
        value = baseValue + modifier
    in
        if value > 9 then value - 9 else value
