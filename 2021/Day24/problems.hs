import System.IO
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

data OpCode = Inp | Add | Mul | Div | Mod | Eql deriving (Show, Enum, Eq)

main = do
    raw <- readFile "input.txt"
    let program = parseInput $ lines raw
    print $ run 0 [] [
        (1, 13, 3),
        (1, 11, 12),
        (1, 15, 9),
        (26, -6, 12),
        (1, 15, 2),
        (26, -8, 1),
        (26, -4, 1),
        (1, 15, 13),
        (1, 10, 1),
        (1, 11, 6),
        (26, -11, 2),
        (26, 0, 11),
        (26, -8, 10),
        (26, -7, 3)]

problem1 :: [(Int, Int, Int)] -> String
problem1 coeffs = fromJust $ run 0 [] coeffs

run :: Int -> [Int] -> [(Int, Int, Int)] -> Maybe String
run z n []
    | z == 0    = Just $ concat $ map (show) $ reverse n
    | otherwise = Nothing

run z n ((1, _, c):rest) =
    let
        eval = dropWhile (== Nothing) $ map (\w -> run (z * 26 + w + c) (w:n) rest) [9,8..1]
    in
        if eval == [] then Nothing else head $ eval

run z n ((26, b, _):rest) =
    let
        eval = dropWhile (== Nothing) $ map (\w -> if z `mod` 26 + b == w then run (z `div` 26) (w:n) rest else Nothing) [9,8..1]
    in
        if eval == [] then Nothing else head $ eval

execute :: [(OpCode, String, String)] -> String -> (String, M.Map String Int)
execute program input =
    let
        state = (input, (M.fromList [("w", 0), ("x", 0), ("y", 0), ("z", 0)]))
    in
        foldl (\s i ->
            traceShow (s, i) $
            executeInstruction i s) state program

executeInstruction :: (OpCode, String, String) -> (String, M.Map String Int) -> (String, M.Map String Int)
executeInstruction (Inp, arg1, _) (d:rest, registers) =
    (rest, M.insert arg1 (digitToInt d) registers)

executeInstruction (Add, arg1, arg2) (input, registers) =
    (input, executeBinaryInstruction arg1 arg2 registers (+))

executeInstruction (Mul, arg1, arg2) (input, registers) =
    (input, executeBinaryInstruction arg1 arg2 registers (*))

executeInstruction (Div, arg1, arg2) (input, registers) =
    (input, executeBinaryInstruction arg1 arg2 registers (div))

executeInstruction (Mod, arg1, arg2) (input, registers) =
    (input, executeBinaryInstruction arg1 arg2 registers (mod))

executeInstruction (Eql, arg1, arg2) (input, registers) =
    (input, executeBinaryInstruction arg1 arg2 registers (\a b -> if a == b then 1 else 0))

executeBinaryInstruction :: String -> String -> M.Map String Int -> (Int -> Int -> Int) -> M.Map String Int
executeBinaryInstruction arg1 arg2 registers fn =
    let
        v1 = extractValue arg1 registers
        v2 = extractValue arg2 registers
        res = fn v1 v2
    in
        M.insert arg1 res registers

extractValue :: String -> M.Map String Int -> Int
extractValue valueOrRegister registers =
    if isLetter $ head valueOrRegister
        then fromJust $ M.lookup valueOrRegister registers
        else read valueOrRegister::Int

parseInput :: [String] -> [(OpCode, String, String)]
parseInput ls = map (\l -> parseLine $ words l) ls

parseLine :: [String] -> (OpCode, String, String)
parseLine ("inp":arg1:_) = (Inp, arg1, "0")
parseLine (opcode:arg1:arg2:_)
    | opcode == "add" = (Add, arg1, arg2)
    | opcode == "mul" = (Mul, arg1, arg2)
    | opcode == "div" = (Div, arg1, arg2)
    | opcode == "mod" = (Mod, arg1, arg2)
    | opcode == "eql" = (Eql, arg1, arg2)