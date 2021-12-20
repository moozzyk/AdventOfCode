import System.IO
import qualified Data.Map as M

main = do
    raw <- readFile "input.txt"
    let algo = head $ lines raw
    let img = parseImage $ drop 2 $ lines raw
    print $ problem1 algo (img, '.')

parseImage :: [String] -> M.Map (Int, Int) Char
parseImage ls =
    M.fromList $ zip ([(x, y) | y <- [0..(length ls) - 1],
                                x <- [0..(length $ head ls) - 1]]) (concat ls)
problem1 :: String -> (M.Map (Int,Int) Char, Char) -> Int
problem1 algo img =
    let
        (pixelMap, _) = transform algo $ transform algo img
    in
        length $ filter (== '#') $ M.elems pixelMap

transform :: String -> (M.Map (Int,Int) Char, Char) -> (M.Map (Int,Int) Char, Char)
transform algo (img, def) =
    let
        minX = minimum $ map (fst) $ M.keys img
        maxX = maximum $ map (fst) $ M.keys img
        minY = minimum $ map (snd) $ M.keys img
        maxY = maximum $ map (snd) $ M.keys img
        coords = [(x, y) | y <- [(minY - 2)..(maxY + 2)],
                           x <- [(minX - 2)..(maxX + 2)]]
        indices = map (\c -> getIndex c (img, def)) coords
        def' = algo !! (indexStrToNum $ replicate 9 def)
        imgLines = getNewImageLines algo indices
    in
        (parseImage imgLines, def')

getNewImageLines :: String -> [Int] -> [String]
getNewImageLines algo indices =
    let
        chars = map (\i -> algo !! i) indices
        size = floor $ sqrt $ fromIntegral $ length chars
    in
        chunksOf size chars

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = [take n xs] ++ (chunksOf n $ drop n xs)

getIndex :: (Int, Int) -> (M.Map (Int,Int) Char, Char) -> Int
getIndex (x, y) img =
    let
        indexStr =
            [imgLookup (x - 1, y - 1) img, imgLookup (x, y - 1) img, imgLookup (x + 1, y - 1) img,
            imgLookup (x - 1, y) img, imgLookup (x, y) img, imgLookup (x + 1, y) img,
            imgLookup (x - 1, y + 1) img, imgLookup (x, y + 1) img, imgLookup (x + 1, y + 1) img]
    in
        indexStrToNum indexStr

indexStrToNum :: String -> Int
indexStrToNum indexStr =
    fst $ foldr (\c (res, mult) -> (res + (if c == '#' then mult else 0), mult * 2)) (0, 1) indexStr

imgLookup :: (Int, Int) -> (M.Map (Int,Int) Char, Char) -> Char
imgLookup coord (img, def) =
   case M.lookup coord img of
        Just v -> v
        Nothing -> def