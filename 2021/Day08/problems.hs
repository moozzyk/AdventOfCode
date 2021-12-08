import System.IO
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let input = parseInput raw
    print $ problem1 input

problem1 :: [([String], [String])] -> Int
problem1 input =
  length $ filter (\i -> (length i) <= 4 || (length i) == 7) $ concat $ map snd input

parseInput :: String -> [([String], [String])]
parseInput raw =
    map splitLine $ lines raw

splitLine :: String -> ([String], [String])
splitLine l =
    let
        w = words l
    in
        (take 10 w, drop 11 w)