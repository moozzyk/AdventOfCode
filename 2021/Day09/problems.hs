import System.IO
import Data.Char
import qualified Data.Map as M
import Data.Maybe

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    print $ problem1 $ getMap input

problem1 heightMap =
    sum $ map (\c -> calculateRisk c heightMap) $ M.keys heightMap

calculateRisk coordinate heightMap =
    if isLowPoint coordinate heightMap
        then 1 + (fromJust $ M.lookup coordinate heightMap)
        else 0

isLowPoint coordinate heightMap =
    let
        (x, y) = coordinate
        height = fromJust $ M.lookup coordinate heightMap
        adjacentHeights = [
            M.findWithDefault 10 (x - 1, y) heightMap,
            M.findWithDefault 10 (x + 1, y) heightMap,
            M.findWithDefault 10 (x, y - 1) heightMap,
            M.findWithDefault 10 (x, y + 1) heightMap ]
    in
        height < minimum adjacentHeights


getMap :: [String] -> M.Map (Int, Int) Int
getMap input =
    let
        coords = [(x, y) | y <- [0..(length input) - 1], x <- [0..(length $ head input) - 1]]
        heights = map digitToInt $ concat input
    in
        M.fromList $ zip coords heights
