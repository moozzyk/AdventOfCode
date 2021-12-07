import System.IO
import Data.List
import Data.Map as Map
import Data.Maybe
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let numbers = fmap (\w -> read w::Int) $ words $ fmap (\c -> if c == ',' then ' ' else c) raw
    print $ problem1 numbers

problem1 :: [Int] -> Int
problem1 numbers =
    let
        sorted = group $ sort numbers
        crabMap = Map.fromList $ fmap (\s -> (head s, length s)) sorted
        positions = [(minimum numbers)..(maximum numbers) + 1]
        costFromLeft = solveFast positions crabMap
        costFromRight = reverse $ solveFast ([(maximum numbers) + 1, (maximum numbers)..(minimum numbers) - 1]) crabMap
        costCombined = zip costFromLeft costFromRight
    in
        minimum $ fmap (\(c1, c2) -> c1 + c2) costCombined

solveFast :: [Int] -> Map Int Int -> [Int]
solveFast positions crabMap =
        tail $ snd $ mapAccumL(\(cost, count) pos -> ((cost + count, count + countAt pos), cost)) (0,0) positions
    where
        countAt pos = fromMaybe 0 (Map.lookup pos crabMap)
