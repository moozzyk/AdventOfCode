import System.IO
import Data.Char
import qualified Data.Map as M
import Data.List

main = do
    raw <- readFile "input.txt"
    let graph = parseInput $ lines raw
    print $ problem1 graph

problem1 :: [(String, String)] -> Int
problem1 graph = length $ visit "start" graph []

visit :: String -> [(String, String)] -> [String] -> [[String]]
visit node graph visited =
    let
        childNodes = (map snd $ filter (\(from, _) -> from == node) graph) \\ visited
        visited' = if (isLowerStr node) then node:visited else visited
    in
        if node == "end" then
            [[node]]
        else
            map (\p -> node:p) $ concat $ map (\n -> visit n graph visited') childNodes

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
