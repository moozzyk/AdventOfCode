import System.IO
import Data.List

main = do
    raw <- readFile "input.txt"
    let ls = map (\l -> syntaxErrorScore l []) $ lines raw
    print $ problem1 ls
    print $ problem2 ls

problem1 :: [(String, Int)] -> Int
problem1 xs =
    sum $ map snd $ filter (\s -> fst s == "corrupted") xs

problem2 :: [(String, Int)] -> Int
problem2 xs =
    let
        result = map snd $ filter (\s -> fst s == "incomplete") xs
        len = length result
    in
        head $ drop (len `div` 2) $ sort result

syntaxErrorScore :: [Char] -> [Char] -> (String, Int)
syntaxErrorScore [] stack = ("incomplete", autocompleteScore stack)
syntaxErrorScore (c:r) stack
    | elem c "([{<"                 = syntaxErrorScore r (c:stack)
    | c == ')' && head stack /= '(' = ("corrupted", 3)
    | c == ']' && head stack /= '[' = ("corrupted", 57)
    | c == '}' && head stack /= '{' = ("corrupted", 1197)
    | c == '>' && head stack /= '<' = ("corrupted", 25137)
    | otherwise                     = syntaxErrorScore r (tail stack)

autocompleteScore :: String -> Int
autocompleteScore s =
    foldl (\acc c -> acc * 5 + autocompleteCharScore c) 0 s

autocompleteCharScore :: Char -> Int
autocompleteCharScore c
    | c == '(' = 1
    | c == '[' = 2
    | c == '{' = 3
    | c == '<' = 4
    | otherwise = -1
