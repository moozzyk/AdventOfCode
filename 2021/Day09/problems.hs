import System.IO
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    let heightMap = getMap input
    print $ problem1 heightMap
    print $ problem2 heightMap

problem1 :: M.Map (Int, Int) Int -> Int
problem1 heightMap =
    sum $ map (\c -> calculateRisk c heightMap) $ M.keys heightMap

calculateRisk :: (Int, Int) -> M.Map (Int, Int) Int -> Int
calculateRisk coordinate heightMap
    | isLowPoint coordinate heightMap = 1 + (fromJust $ M.lookup coordinate heightMap)
    | otherwise                       = 0

problem2 :: M.Map (Int, Int) Int -> Int
problem2 heightMap =
    product $ take 3 $ sortBy (comparing Down) $ map (\c -> basinSize c heightMap) $ M.keys heightMap

basinSize :: (Int, Int) -> M.Map (Int, Int) Int -> Int
basinSize coordinate heightMap
    | isLowPoint coordinate heightMap = length $ basin coordinate heightMap (S.fromList [])
    | otherwise                       = 0

basin :: (Int, Int) -> M.Map (Int, Int) Int -> S.Set (Int, Int) -> S.Set (Int, Int)
basin coordinate heightMap visited =
    let
        (x, y) = coordinate
    in
        if M.findWithDefault 9 coordinate heightMap == 9 ||
            S.member coordinate visited
        then
            visited
        else
            basin (x, y - 1) heightMap $
            basin (x, y + 1) heightMap $
            basin (x + 1, y) heightMap $
            basin (x - 1, y) heightMap $ S.insert coordinate visited

isLowPoint :: (Int, Int) -> M.Map (Int, Int) Int -> Bool
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
