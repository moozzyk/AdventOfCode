import System.IO
import Data.Char
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let ls = lines raw
    print $ problem1 ls
    print $ problem2 ls

problem1 :: [String] -> Int
problem1 ls = magnitude $ foldl (\res n -> addNumbers res n) (head ls) (tail ls)

problem2 :: [String] -> Int
problem2 ls =
    maximum $ map magnitude $ map (\(n1, n2) -> addNumbers n1 n2) $ [(x,y) | x<-ls, y<-ls]

magnitude :: String -> Int
magnitude (i:n) =
    let
        (left, right) = splitToParts [] (init n) 0
        magL = magnitude left
        magR = magnitude right
    in
        if isDigit(i) then digitToInt i else (magL * 3 + magR * 2)

splitToParts :: String -> String -> Int -> (String, String)
splitToParts seen (c:rest) level
    | c == ',' && level == 0 = (seen, rest)
    | c == '[' = splitToParts (seen ++ [c]) rest (level + 1)
    | c == ']' = splitToParts (seen ++ [c]) rest (level - 1)
    | otherwise = splitToParts (seen ++ [c]) rest level

addNumbers :: String -> String -> String
addNumbers n1 n2 =
    let
        n' = "[" ++ n1 ++ "," ++ n2 ++ "]"
        result = reduceNumber n'
    in
        result

reduceNumber :: String -> String
reduceNumber n =
    let
        n' = reduceExpRepeatedly n
        n'' = reduceSplit [] n'
    in
        if n'' == n
            then n''
            else reduceNumber n''

reduceExpRepeatedly :: String -> String
reduceExpRepeatedly n =
    let
        n' = reduceExp [] n 0
    in
        if n' == n
            then n'
            else reduceExpRepeatedly n'

reduceExp :: String -> String -> Int -> String
reduceExp seen [] _ = seen
reduceExp seen ('[':rest) 4 = explode seen ('[':rest)
reduceExp seen ('[':rest) level = reduceExp (seen ++ "[") rest (level + 1)
reduceExp seen (']':rest) level = reduceExp (seen ++ "]") rest (level - 1)
reduceExp seen (c:rest) level = reduceExp (seen ++ [c]) rest level

explode :: String -> String -> String
explode seen rest =
    let
        n1 = takeWhile isDigit $ tail rest
        n2 = takeWhile isDigit $ drop (2 + (length n1)) rest
        l = (length n1) + (length n2) + 3 -- 3 -> [ , ]
        f = reverse $ addNumberRev [] n1 $ reverse $ seen
        b = addNumber [] n2 $ drop l rest
    in
        f ++ "0" ++ b

reduceSplit :: String -> String -> String
reduceSplit seen [] = seen
reduceSplit seen (c:rest)
    | (isDigit c) && (isDigit $ head rest) = split seen (c:rest)
    | otherwise                            = reduceSplit (seen ++ [c]) rest

split :: String -> String -> String
split processed rest =
    let
        n = takeWhile isDigit rest
        n' = read n::Int
        l = show $ n' `div` 2
        r = show $ abs $ (-n') `div` 2
    in
        processed ++ "[" ++ l ++ "," ++ r ++ "]" ++ drop (length n) rest

addNumber :: String -> String -> String -> String
addNumber processed _ [] = processed
addNumber processed n (i:rest)
    | i == '[' || i == ']' || i == ',' = addNumber (processed ++ [i]) n rest
    | otherwise = addNumber' processed n (i:rest)

addNumber' :: String -> String -> String -> String
addNumber' processed n rest =
        let
            tStr = takeWhile isDigit rest
            n' = (read tStr::Int) + (read n::Int)
        in
            processed ++ (show n') ++ drop (length tStr) rest

addNumberRev :: String -> String -> String -> String
addNumberRev processed _ [] = processed
addNumberRev processed n (i:rest)
    | i == '[' || i == ']' || i == ',' = addNumberRev (processed ++ [i]) n rest
    | otherwise = addNumberRev' processed n (i:rest)

addNumberRev' :: String -> String -> String -> String
addNumberRev' processed n rest =
        let
            tStr = reverse $ takeWhile isDigit rest
            n' = (read tStr::Int) + (read n::Int)
        in
            processed ++ (reverse $ show n') ++ drop (length tStr) rest