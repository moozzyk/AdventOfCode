import System.IO
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let l = lines raw
    let numbers = parseNumbers $ head l
    let numIndexPairs = zip numbers [1..]
    let boards = getBoards $ tail l
    let (winningBoard, numRounds) = playBingos boards numIndexPairs
    let r = calculateScore winningBoard (take numRounds numbers)
    print r

parseNumbers :: String -> [Int]
parseNumbers s = map (\w -> read w::Int) $ words $ map (\c -> if c == ',' then ' ' else c) s

getBoards :: [String] -> [[[Int]]]
getBoards s =
    let
        initialBoards = parseBoards s
    in
        initialBoards ++ map transpose initialBoards

parseBoards :: [String] -> [[[Int]]]
parseBoards [] = []
parseBoards xs = ([parseBoard . take 5 $ tail xs]) ++ (parseBoards $ drop 6 xs)

parseBoard :: [String] ->[[Int]]
parseBoard xs = map parseNumbers xs

playBingos :: [[[Int]]] -> [(Int, Int)] -> ([[Int]], Int)
playBingos boards numIndexPairs = minimumBy (comparing snd) $ map (\b -> (b, playBingo b numIndexPairs)) boards

playBingo :: [[Int]] -> [(Int, Int)] -> Int
playBingo board numIndexPairs = minimum $ map (\boardRow -> numRounds boardRow numIndexPairs) board

numRounds :: [Int] ->[(Int, Int)] -> Int
numRounds boardRow numIndexPairs = fromJust(maximum $ map (\n -> lookup n numIndexPairs) boardRow)

calculateScore :: [[Int]] -> [Int] -> Int
calculateScore board numbers = last numbers * (sum $ concat board \\ numbers)