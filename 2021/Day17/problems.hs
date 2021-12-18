import Data.List
import Debug.Trace

main = do
    let area = ((14, 50), (-225, -267))
    print $ problem1 area

problem1 :: ((Int, Int), (Int, Int)) -> Int
problem1 ((x1, x2), (y1, y2)) =
    let
        candidates = [(dx, dy) | dx <- [0..(max x1 x2)],
                                 dy <- [0..(max (abs y1) (abs y2))]]
        routes = map (\vel -> simulate (0, 0) vel y2) candidates
        valid = filter (\p -> anyHit p ((x1, x2), (y1, y2))) routes
    in
        maximum $ map snd $ concat $ valid

simulate :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
simulate (x, y) (dx, dy) minY
    | y < minY = []
    | otherwise = (x, y):(simulate (x + dx, y + dy) (max (dx - 1) 0, dy - 1) minY)

anyHit :: [(Int, Int)] -> ((Int, Int), (Int, Int)) -> Bool
anyHit positions targetArea =
    any (\pos -> hit pos targetArea) positions

hit :: (Int, Int) -> ((Int, Int), (Int, Int)) -> Bool
hit (x, y) ((x1, x2), (y1, y2)) =
    x >= x1 && x <= x2 && y <= y1 && y >= y2
