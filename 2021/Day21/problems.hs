import System.IO
import Data.Maybe
import Debug.Trace

main = do
    let pos1 = 10
    let pos2 = 2
    print $ problem1 pos1 pos2
    print $ problem2 pos1 pos2

problem1 :: Int -> Int -> Int
problem1 pos1 pos2 =
    play (pos1, 0) (pos2, 0) 1 1

play :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int
play (pos1, score1) (pos2, score2) die turn =
    let
        roll = rollDie die
        pos1' = addSpecial pos1 roll 10
        score1' = (score1 + pos1')
        die' = addSpecial die 3 100
    in
        if score1' >= 1000
            then score2 * turn * 3
            else play (pos2, score2) (pos1', score1') die' (turn + 1)

rollDie :: Int -> Int
rollDie die
    | die == 99 = 200
    | die == 100 = 103
    | otherwise = 3 * die + 3

addSpecial :: Int -> Int -> Int -> Int
addSpecial n1 n2 maxVal =
    let candidate = (n1 + n2) `mod` maxVal
    in if candidate == 0 then maxVal else candidate

problem2 :: Int -> Int -> Int
problem2 pos1 pos2 =
    let
        (p1, p2) = simulate (pos1, 0) (pos2, 0) 1
    in
        traceShow (p1, p2) $
        max p1 p2

simulate :: (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
simulate (pos1, score1) (pos2, score2) player =
    if player == 1 then
        let
            moves = map (\fwd -> move pos1 score1 fwd) [3..9]
            partialResults = map (\(nextPos, score, numWays) -> times numWays
                (if score >= 21
                    then (1, 0)
                    else simulate (nextPos, score) (pos2, score2) 2)) moves
        in
            foldr (plus) (0, 0) partialResults
    else
        let
            moves = map (\fwd -> move pos2 score2 fwd) [3..9]
            partialResults = map (\(nextPos, score, numWays) -> times numWays
                (if score >= 21
                    then (0, 1)
                    else simulate (pos1, score1) (nextPos, score) 1)) moves
        in
            foldr (plus) (0, 0) partialResults

move :: Int -> Int -> Int -> (Int, Int, Int)
move pos score fwd =
    let
        nextPos = addSpecial pos fwd 10
        score' = score + nextPos
    in
        (nextPos, score', getNumWays fwd)

times :: Int -> (Int, Int) -> (Int, Int)
times n (x, y) = (n * x, n * y)

plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
plus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getNumWays :: Int -> Int
getNumWays fwd
    | fwd == 3 = 1
    | fwd == 4 = 3
    | fwd == 5 = 6
    | fwd == 6 = 7
    | fwd == 7 = 6
    | fwd == 8 = 3
    | fwd == 9 = 1
