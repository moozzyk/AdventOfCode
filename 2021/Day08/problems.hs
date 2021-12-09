import System.IO
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let input = parseInput raw
    print $ problem1 input
    print $ problem2 input

problem1 :: [([String], [String])] -> Int
problem1 input =
  length $ filter (\i -> (length i) <= 4 || (length i) == 7) $ concat $ map snd input

problem2 :: [([String], [String])] -> Int
problem2 ls =
    sum $ map getValue ls

getValue :: ([String], [String]) -> Int
getValue l =
    decodeNumber (snd l) (createDecodeMap $ fst l)

decodeNumber :: [String] -> M.Map Char Char -> Int
decodeNumber values decodeMap =
    read (fmap (\v -> decodeDigit v decodeMap) values)::Int

decodeDigit :: String -> M.Map Char Char -> Char
decodeDigit value decodeMap =
    getDigit $ sort $ fmap (\c -> fromJust $ M.lookup c decodeMap) value

getDigit :: String -> Char
getDigit n =
    let
        numbers = M.fromList[
            ("abcefg", '0'), ("cf", '1'), ("acdeg", '2'), ("acdfg", '3'),
            ("bcdf", '4'), ("abdfg", '5'), ("abdefg", '6'), ("acf", '7'),
            ("abcdefg", '8'), ("abcdfg", '9')
            ]
    in
        fromJust $ M.lookup n numbers

createDecodeMap :: [String] -> M.Map Char Char
createDecodeMap patterns =
    let
        segmentMap = decodeB patterns $decodeCF patterns $
            decodeD patterns $ decodeGE patterns $ decodeA patterns
    in
        M.fromList $ sort $ fmap (\(k, v) -> (v, k)) $ M.toList segmentMap

decodeA :: [String] -> M.Map Char Char
decodeA patterns =
    let
        one = findDigit 2 patterns
        seven = findDigit 3 patterns
    in
        M.insert 'a' (head $ seven \\ one) M.empty

decodeGE :: [String] -> M.Map Char Char -> M.Map Char Char
decodeGE patterns segmentMap =
    let
        four = findDigit 4 patterns
        almostNine = (fromJust $ M.lookup 'a' segmentMap):four
        nine = fromJust $ find (\p -> length p == 6 && (length $ p \\ almostNine) == 1) patterns
        eight = findDigit 7 patterns
    in
        M.insert 'e' (head $ eight \\ nine) $
        M.insert 'g' (head $ nine \\ almostNine) segmentMap

decodeD :: [String] -> M.Map Char Char -> M.Map Char Char
decodeD patterns segmentMap =
    let
        seven = findDigit 3 patterns
        almostThree = (fromJust $ M.lookup 'g' segmentMap):seven
        three = fromJust $ find (\p -> length p == 5 && (length $p \\ almostThree) == 1) patterns
    in
        M.insert 'd' (head $ three \\ almostThree) segmentMap

decodeCF :: [String] -> M.Map Char Char -> M.Map Char Char
decodeCF patterns segmentMap =
    let
        one = findDigit 2 patterns
        almostTwo = M.elems segmentMap
        two = fromJust $ find (\p -> length p == 5 && (length $p \\ almostTwo) == 1) patterns
        c = two \\ almostTwo
        f = one \\ c
    in
        M.insert 'c' (head $ c) $
        M.insert 'f' (head $ f) segmentMap

decodeB :: [String] -> M.Map Char Char -> M.Map Char Char
decodeB patterns segmentMap =
    let
        one = findDigit 2 patterns
        four = findDigit 4 patterns
        d = fromJust $ M.lookup 'd' segmentMap
        b = head $ (four \\ one) \\ [d]
    in
        M.insert 'b' b segmentMap

findDigit :: Int -> [String] -> String
findDigit l patterns =
    fromJust $ find (\i -> (length i) == l) patterns

parseInput :: String -> [([String], [String])]
parseInput raw =
    map splitLine $ lines raw

splitLine :: String -> ([String], [String])
splitLine l =
    let
        w = words l
    in
        (take 10 w, drop 11 w)