import System.IO
import Data.List
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let numbers = parseInput raw
    let school = take 9 $ [0] ++ (map (\i -> length i) $ group $ sort numbers) ++ [0,0,0,0,0,0,0,0]
    print $ problem1 school
    print $ problem2 school

problem1 :: [Int] -> Int
problem1 school =
    run school 80

problem2 :: [Int] -> Int
problem2 school =
    run school 256

run :: [Int] -> Int -> Int
run school days =
    if days == 0 then sum(school) else run (day school) (days -1)

day :: [Int] -> [Int]
day school =
    let
        zeroth = head school
        seventh = head $ drop 7 school
        eighth = last school
    in
        (take 6 $ drop 1 school) ++ [seventh + zeroth] ++ [eighth, zeroth]

parseInput :: String -> [Int]
parseInput input =
    map (\w -> read w::Int) $ words $ map (\c -> if c == ',' then ' ' else c) input