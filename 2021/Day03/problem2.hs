import System.IO
import Data.List
import Data.Function
import Data.Char
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let input = lines raw
    let oxygen = findWithPrefix input "" mostCommon
    let co2 = findWithPrefix input "" leastCommon
    print (toDec(oxygen) * toDec(co2))

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

mostCommon :: String -> Char
mostCommon s = head $ maximumBy (compare `on` length) $ group $ sort s

leastCommon :: String -> Char
leastCommon s = head $ minimumBy (compare `on` length) $ group $ sort s

findWithPrefix :: [String] -> String -> (String -> Char) -> String
findWithPrefix [x] _ _ = x

findWithPrefix xs p f =
    let candidates = filter (\s -> p `isPrefixOf` s) xs
        suffix = f $ head $ drop (length p) $ transpose candidates
    in
        findWithPrefix candidates (p ++ [suffix]) f
