import System.IO
import Data.List
import Data.Function
import Data.Char

main = do
    raw <- readFile "input.txt"
    let input = lines raw
        t = transpose input
        n = map mostCommon t
        n1 = map (\c -> if c == '1' then '0' else '1') n
        result = (toDec n) * (toDec n1)
    print result

mostCommon :: String -> Char
mostCommon s = head $ maximumBy (compare `on` length) $ group $ sort s

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0