import System.IO
import Data.List

main = do
    raw <- readFile "input.txt"
    let input = map readInt . words $ raw
    let tmpSums = map (\t -> sum t) $ windows 3 input
    -- drop last two as they are partial sums
    let sums = take (length tmpSums - 2) tmpSums
    let pairs = zip sums (tail sums)
    print . sum $ map (\(a,b) -> fromEnum (b > a)) pairs

readInt :: String -> Int
readInt = read

windows :: Int -> [a] ->[[a]]
windows n xs = Data.List.transpose (take n (tails xs))
