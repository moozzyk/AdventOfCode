import System.IO
import qualified Data.Map as M
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let sea = parseInput $ lines raw
    print $ problem1 sea

problem1 :: (Int, Int, [((Int, Int), Char)]) -> Int
problem1 sea = run sea 1

run :: (Int, Int, [((Int, Int), Char)]) -> Int -> Int
run (maxX, maxY, c) i =
    let
        c' = step (maxX, maxY, c)
    in
        if c == c'
            then i
            else run (maxX, maxY, c') (i + 1)

step :: (Int, Int, [((Int, Int), Char)]) -> [((Int, Int), Char)]
step (maxX, maxY, cucumbers) =
    stepVertical maxY $ stepHorizontal maxX cucumbers

stepHorizontal :: Int -> [((Int, Int), Char)] -> [((Int, Int), Char)]
stepHorizontal maxX cucumbers =
    let
        hCucumbers = filter (\(_, c) -> c == '>') cucumbers
        vCucumbers = filter (\(_, c) -> c == 'v') cucumbers
        m = M.fromList cucumbers
    in
        vCucumbers ++ map (\c -> newX maxX c m) hCucumbers

newX :: Int -> ((Int, Int), Char) -> M.Map (Int, Int) Char-> ((Int, Int), Char)
newX maxX ((x, y), c) m =
    let
        x' = (x + 1) `mod` (maxX + 1)
    in
        if M.lookup (x', y) m == Nothing
            then ((x', y), c)
            else ((x, y), c)

stepVertical :: Int -> [((Int, Int), Char)] -> [((Int, Int), Char)]
stepVertical maxY cucumbers =
    let
        hCucumbers = filter (\(_, c) -> c == '>') cucumbers
        vCucumbers = filter (\(_, c) -> c == 'v') cucumbers
        m = M.fromList cucumbers
    in
        hCucumbers ++ map (\c -> newY maxY c m) vCucumbers

newY :: Int -> ((Int, Int), Char) -> M.Map (Int, Int) Char-> ((Int, Int), Char)
newY maxY ((x, y), c) m =
    let
        y' = (y + 1) `mod` (maxY + 1)
    in
        if M.lookup (x, y') m == Nothing
            then ((x, y'), c)
            else ((x, y), c)

parseInput :: [String] -> (Int, Int, [((Int, Int), Char)])
parseInput ls =
    let
        maxX = (length $ head ls) - 1
        maxY = (length ls) - 1
        coords = [(x, y) | y <- [0..maxY], x <- [0..maxX]]
        cucumbers = filter (\(_, i) -> i /= '.') $ zip coords $ concat ls
    in
        (maxX, maxY, cucumbers)
