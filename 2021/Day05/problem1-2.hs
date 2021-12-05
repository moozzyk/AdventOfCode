import System.IO
import Data.List
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    let coords = map parseLine input
    let p1 = problem $ filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) coords
    let p2 = problem coords
    print p1
    print p2

problem coords =
    length $ filter (\s -> length s /= 1) $ group $ sort $
    concat $ map linePoints coords

linePoints ((x1, y1),(x2, y2)) =
    let
        dx = if x1 < x2 then 1 else if x1 > x2 then -1 else 0
        dy = if y1 < y2 then 1 else if y1 > y2 then -1 else 0
    in
        if x1 == x2 && y1 == y2 then [(x1, y1)]
        else (x1, y1):linePoints ((x1 + dx, y1 + dy), (x2, y2))

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine l =
    let
        w = words l
    in
        (parsePair $ head w, parsePair $ last w)

parsePair :: String -> (Int, Int)
parsePair p =
    let
        c = map (\w -> read w::Int) $ words $ map (\c -> if c == ',' then ' ' else c) p
    in
        (head c, last c)
