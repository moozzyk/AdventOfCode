import System.IO
import Data.List
import Data.Maybe
import Data.Ord
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let l = lines raw
    let numbers = parseNumbers $ head l
    let boards = parseBoards $ tail l
    let p1 = problem1 boards numbers
    let p2 = problem2 boards numbers
    print p1
    print p2

problem1 inputBoards numbers =
    let
        numIndexPairs = zip numbers [1..]
        boards = inputBoards ++ map transpose inputBoards
        candidates = playBingos boards numIndexPairs
        (winningBoard, numRounds) = winningBingo candidates
    in
        calculateScore winningBoard (take numRounds numbers)

problem2 inputBoards numbers =
    let
        numIndexPairs = zip numbers[1..]
        losingBoardsHor = playBingos inputBoards numIndexPairs
        losingBoardsVer = playBingos (map transpose inputBoards) numIndexPairs
        losingBoardPairs = zip losingBoardsHor losingBoardsVer
        candidates = map (\(b1, b2) -> if snd b1 < snd b2 then b1 else b2) losingBoardPairs
        (losingBoard, numRounds) = losingBingo candidates
    in
        calculateScore losingBoard (take numRounds numbers)


parseNumbers :: String -> [Int]
parseNumbers s = map (\w -> read w::Int) $ words $ map (\c -> if c == ',' then ' ' else c) s

parseBoards :: [String] -> [[[Int]]]
parseBoards [] = []
parseBoards xs = ([parseBoard . take 5 $ tail xs]) ++ (parseBoards $ drop 6 xs)

parseBoard :: [String] ->[[Int]]
parseBoard xs = map parseNumbers xs

winningBingo :: [([[Int]], Int)] -> ([[Int]], Int)
winningBingo candidates = minimumBy (comparing snd) candidates

losingBingo :: [([[Int]], Int)] -> ([[Int]], Int)
losingBingo candidates = maximumBy (comparing snd) candidates

playBingos :: [[[Int]]] -> [(Int, Int)] -> [([[Int]], Int)]
playBingos boards numIndexPairs = map (\b -> (b, playBingo b numIndexPairs)) boards

playBingo :: [[Int]] -> [(Int, Int)] -> Int
playBingo board numIndexPairs = minimum $ map (\boardRow -> numRounds boardRow numIndexPairs) board

numRounds :: [Int] ->[(Int, Int)] -> Int
numRounds boardRow numIndexPairs = fromJust(maximum $ map (\n -> lookup n numIndexPairs) boardRow)

calculateScore :: [[Int]] -> [Int] -> Int
calculateScore board numbers = last numbers * (sum $ concat board \\ numbers)