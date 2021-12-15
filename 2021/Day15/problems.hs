import System.IO
import Data.Char
import qualified Data.Map as M
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let grid = M.fromList  $ parseInput $ lines raw
    print $ problem1 grid

parseInput :: [String] -> [((Int, Int), Int)]
parseInput ls =
    let
        side = (length $ head ls)-1
        coords = [(x, y) | x <- [0..side], y <- [0..side]]
    in  zip coords $ map digitToInt $ concat ls

problem1 :: M.Map (Int, Int) Int -> Int
problem1 grid =
    let
        side = maximum $ map snd $ M.keys grid
        coords = [(x, y) | x <- [0..side], y <- [0..side]]
        dp = foldl (\d coord -> M.insert coord (riskAt coord grid d) d) M.empty coords
    in
        safeLookup (side, side) dp - safeLookup (0, 0) grid

riskAt :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int -> Int
riskAt (0, 0) grid _ = safeLookup (0, 0) grid
riskAt (x, y) grid dp =
    safeLookup (x, y) grid +
    (min (safeLookup (x - 1, y) dp) (safeLookup (x, y - 1) dp))

safeLookup :: (Int, Int) -> M.Map (Int, Int) Int -> Int
safeLookup coord grid =
   case M.lookup coord grid of
        Just v -> v
        Nothing -> 1000

