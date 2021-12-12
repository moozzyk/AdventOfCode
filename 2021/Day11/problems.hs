import System.IO
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let grid = M.fromList  $ parseInput $ lines raw
    print $ problem1 grid

parseInput :: [String] -> [((Int, Int), Int)]
parseInput ls =
    let coords = [(x, y) | y <- [0..9], x <- [0..9]]
    in  zip coords $ map digitToInt $ concat ls

problem1 :: M.Map (Int, Int) Int -> Int
problem1 grid =
   sum $ map count0s $ flashN 100 grid

flashN :: Int -> M.Map (Int, Int) Int -> [M.Map (Int, Int) Int]
flashN n grid =
    foldr (\i g -> (flashCycle $ head g):g) [grid] [1..n]

count0s :: M.Map (Int, Int) Int -> Int
count0s grid =
    length $ filter (==0) $ M.elems grid

flashCycle :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
flashCycle grid =
    flashGrid $ increaseEnegry grid

increaseEnegry :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
increaseEnegry grid =
    M.map (+1) grid

flashGrid :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
flashGrid grid =
    let
        coordsToFlash = M.keys $ M.filter (> 9) grid
        grid' = M.mapWithKey (\(coord) val -> if val > 9 then 0 else val) grid
    in
        if coordsToFlash == []
            then grid'
            else flashGrid $ flashForCoords coordsToFlash grid'

flashForCoords :: [(Int, Int)] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
flashForCoords coords grid =
    foldr (\c g -> flashAt c g) grid coords

flashAt :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
flashAt (x , y) grid =
    inc (x - 1, y - 1) $ inc (x, y - 1) $ inc (x + 1, y - 1) $
    inc (x - 1, y) $ inc (x + 1, y) $
    inc (x - 1, y + 1) $ inc (x, y + 1) $ inc (x + 1, y + 1) grid

inc :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
inc (x, y) grid
    | (x < 0) || (y < 0)                  = grid
    | (x > 9) || (y > 9)                  = grid
    | fromJust(M.lookup (x, y) grid) == 0 = grid
    | otherwise                           = M.insert (x, y) (1 + fromJust(M.lookup (x, y) grid)) grid

gridToStr :: M.Map (Int, Int) Int -> [String]
gridToStr grid =
    let
        coords = [(x, y) | y <- [0..9], x <- [0..9]]
        cs = map (\c -> fromJust $ M.lookup c grid) coords
    in
        getGridToStr $ map intToDigit cs

getGridToStr :: String -> [String]
getGridToStr [] = []
getGridToStr cs =
    traceShow (take 10 cs) $
    [take 10 cs] ++ (getGridToStr $ drop 10 cs)
