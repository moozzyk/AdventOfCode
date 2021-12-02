import System.IO

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    let s = map (\s -> words s) input
    let (pos, depth) = foldl sumTuples (0, 0) $ map toTuple s
    print (pos * depth)

toTuple :: [String] -> (Int, Int)
toTuple (cmd:units:_)
    | cmd == "forward" = (read units::Int, 0)
    | cmd == "up" = (0, - read units::Int)
    | cmd == "down" = (0, read units::Int)

sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
