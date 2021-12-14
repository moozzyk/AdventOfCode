import System.IO
import Data.List
import qualified Data.Map as M
import Data.Maybe

main = do
    raw <- readFile "input.txt"
    let template = head $ lines raw
    let rules = M.fromList $ map(\l -> (take 2 l, last l)) $ drop 2 $ lines raw
    print $ problem1 template rules

problem1 :: String -> M.Map String Char -> Int
problem1 template rules =
    let
        polymer = substituteN template rules 10
        -- https://stackoverflow.com/questions/3710976/counting-unique-elements-in-a-list
        freq = map snd $ map (\xs@(x:_) -> (x, length xs)) $ group $ sort $ polymer
    in
        maximum freq - minimum freq

substituteN :: String -> M.Map String Char -> Int -> String
substituteN template rules n =
    foldr (\_ t -> substitute t rules) template [1..n]

substitute :: String -> M.Map String Char -> String
substitute [x] rules = [x]
substitute (x:xs) rules =
    let
        s = [x, head xs]
        r = fromJust $ M.lookup s rules
    in
        [x, r] ++ substitute xs rules

