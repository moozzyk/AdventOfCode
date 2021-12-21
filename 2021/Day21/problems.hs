import System.IO
import Debug.Trace

main = do
    let pos1 = 10
    let pos2 = 2
    print $ problem1 pos1 pos2

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