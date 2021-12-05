import System.IO
import Data.List

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    let coords = map parseLine input
    print $ problem coords

problem coords =
    length $ filter (\s -> length s /= 1) $ group $ sort $
    concat $ map linePoints $ filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) coords

linePoints ((x1, y1),(x2, y2)) =
    if x1 == x2 && y1 == y2 then [(x1, y1)]
    else if x1 == x2 then
        if y2 > y1 then
            (x1, y1):linePoints((x1, y1 + 1), (x2, y2))
        else
            (x1, y1):linePoints((x1, y1 - 1), (x2, y2))
    else if y1 == y2 then
        if x2 > x1 then
            (x1, y1):linePoints((x1 + 1, y1), (x2, y2))
        else
            (x1, y1):linePoints((x1 - 1, y1), (x2, y2))
    else []

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
