import System.IO

main = do
    raw <- readFile "input.txt"
    let l = lines raw
    print $ problem1 l

problem1 :: [String] -> Int
problem1 ls =
   sum $ map (\l -> syntaxErrorScore l []) ls

syntaxErrorScore :: [Char] -> [Char] -> Int
syntaxErrorScore [] stack = 0
syntaxErrorScore (c:r) stack
    | elem c "([{<"                 = syntaxErrorScore r (c:stack)
    | c == ')' && head stack /= '(' = 3
    | c == ']' && head stack /= '[' = 57
    | c == '}' && head stack /= '{' = 1197
    | c == '>' && head stack /= '<' = 25137
    | otherwise                     = syntaxErrorScore r (tail stack)

