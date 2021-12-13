import System.IO
import Data.Char
import Data.List

main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    let coords = parseCoords ls
    let folds = parseFolds ls
    print $ problem1 coords folds

problem1 :: [(Int, Int)] -> [(Char, Int)] -> Int
problem1 coords folds =
    length $ fold coords (head folds)

fold :: [(Int, Int)] -> (Char, Int) -> [(Int, Int)]
fold coords ('y', pos) =
    nub $ map (\(x, y) -> if y > pos then (x, pos - (y - pos)) else (x, y)) coords

fold coords ('x', pos) =
    nub $ map (\(x, y) -> if x > pos then (pos - (x - pos), y) else (x, y)) coords

parseCoords :: [String] -> [(Int, Int)]
parseCoords ls =
    map (parseCoord) $ takeWhile (/= []) ls

parseCoord :: String -> (Int, Int)
parseCoord l =
    let nums = map (\i -> read i::Int) $ words $ map (\c -> if c == ',' then ' ' else c) l
    in (head nums, last nums)

parseFolds :: [String] -> [(Char, Int)]
parseFolds ls =
    map parseFold $ tail $ dropWhile (/= []) ls

parseFold :: String -> (Char, Int)
parseFold l =
    let f = drop 11 l
    in (head f, read (drop 2 f)::Int)