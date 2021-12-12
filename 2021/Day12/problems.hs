import System.IO
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

main = do
    raw <- readFile "input.txt"
    let graph = parseInput $ lines raw
    print $ problem1 graph
    print $ problem2 graph

problem1 :: [(String, String)] -> Int
problem1 graph = length $ visit "start" graph [] False

problem2 :: [(String, String)] -> Int
problem2 graph = length $ S.fromList $ visit "start" graph [] True

visit :: String -> [(String, String)] -> [String] -> Bool -> [[String]]
visit node graph visited canSkipSmall =
    let
        childNodes = (map snd $ filter (\(from, _) -> from == node) graph) \\ visited
        visited' = if (isLowerStr node) then node:visited else visited
    in
        if node == "end" then
            [[node]]
        else if canSkipSmall then
            (map (\p -> node:p) $ concat $ map (\n -> visit n graph visited False) childNodes) ++
            (map (\p -> node:p) $ concat $ map (\n -> visit n graph visited' True) childNodes)
        else
            map (\p -> node:p) $ concat $ map (\n -> visit n graph visited' False) childNodes

isLowerStr :: String -> Bool
isLowerStr s = all (\c -> isLower c) s

parseInput :: [String] -> [(String, String)]
parseInput ls = concat $ map parseLine ls

parseLine :: String -> [(String, String)]
parseLine l = createNode $ words $ map (\c -> if c == '-' then ' ' else c) l

createNode :: [String] -> [(String, String)]
createNode (f:t:_)
    | f == "start" = [(f, t)]
    | otherwise    = [(f, t), (t, f)]
