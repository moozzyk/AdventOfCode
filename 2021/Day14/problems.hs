import System.IO
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

main = do
    raw <- readFile "input.txt"
    let template = head $ lines raw
    let rules = M.fromList $ map(\l -> (take 2 l, last l)) $ drop 2 $ lines raw
    print $ problem1 template rules
    print $ problem2 template rules

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

problem2 :: String -> M.Map String Char -> Int
problem2 template rules =
    let
        targetLevel = 40
        pairs = splitTemplate template
        counts = map (\(l, r) -> fromJust $ M.lookup ([l, r], targetLevel) $ getCounts l r rules targetLevel M.empty) pairs
        mergedCounts = foldr (\curr c -> mergeCounts curr c) M.empty counts
        adjustedCounts = foldr (\curr c -> mergeCounts (M.fromList [(curr, -1)]) c) mergedCounts $ tail $ init template
        vals = M.elems adjustedCounts
    in
        (maximum vals) - (minimum vals)

splitTemplate :: String -> [(Char, Char)]
splitTemplate (x:y:[]) = [(x, y)]
splitTemplate (x:y:rest) = ((x, y): splitTemplate (y:rest))

count :: Char -> Char ->  M.Map String Char -> Int -> M.Map (String, Int) (M.Map Char Int) -> M.Map (String, Int) (M.Map Char Int)
count l r _ 0 counts
    | l == r    = M.insert ([l, l], 0) (M.fromList [(l, 2)]) counts
    | otherwise = M.insert ([l, r], 0) (M.fromList [(l, 1), (r, 1)]) counts

count l r rules level counts =
   case M.lookup ([l, r], level) counts of
        Just v -> counts
        Nothing -> getCounts l r rules level counts

getCounts :: Char -> Char -> M.Map String Char -> Int -> M.Map (String, Int) (M.Map Char Int) -> M.Map (String, Int) (M.Map Char Int)
getCounts l r rules level counts =
    let
        m = fromJust $ M.lookup [l, r] rules
        counts' = count l m rules (level - 1) $ count m r rules (level - 1) counts
        lCounts = fromJust $ M.lookup ([l, m], (level - 1)) counts'
        rCounts = fromJust $ M.lookup ([m, r], (level - 1)) counts'
        mergedCounts = mergeCounts (M.fromList [(m, -1)]) $ mergeCounts lCounts rCounts
    in
        M.insert ([l, r], level) mergedCounts counts'

mergeCounts :: M.Map Char Int -> M.Map Char Int -> M.Map Char Int
mergeCounts = M.unionWith (+)

substituteN' :: String -> M.Map String Char -> Int -> [String]
substituteN' template rules n =
    foldr (\_ t -> t ++ [(substitute (last t) rules)]) [template] [1..n]
