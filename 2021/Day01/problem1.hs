import System.IO

main = do
    raw <- readFile "input.txt"
    let input = map readInt . words $ raw
    let pairs = zip input (tail input)
    let res = map (\(a,b) -> fromEnum (b > a)) pairs
    let final = foldl (+) 0 res
    print final

readInt :: String -> Int
readInt = read
