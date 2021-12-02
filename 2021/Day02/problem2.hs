import System.IO

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    let s = map (\s -> words s) input
    let (pos, _, depth) = foldl processTuples (0, 0, 0) $ map toTuple s
    print (pos * depth)

toTuple :: [String] -> (Int, Int, Int)
toTuple (cmd:units:_)
    | cmd == "forward" = (read units::Int, 0, 0)
    | cmd == "up" = (0, - read units::Int, 0)
    | cmd == "down" = (0, read units::Int, 0)

processTuples :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
processTuples (p, a, d) (dp, da, _) = (p + dp, a + da, d + dp * a)
