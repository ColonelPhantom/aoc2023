module Day10 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Debug.Trace (traceShowId, traceShow)
import Data.List (sort)

type Neighs a = a -> [a]

bfs :: Ord k => Neighs k -> [(k, Int)] -> M.Map k Int -> M.Map k Int
bfs f [] visited = visited
bfs f ((q, dist):qs) visited = if new
    then bfs f queue' visited'
    else bfs f qs visited
    where
        new = M.notMember q visited
        neighs = f q
        visited' = M.insert q dist visited
        queue' = qs ++ map (,dist+1) neighs

type Coord = (Int, Int)

inpToMap :: [[Char]] -> M.Map Coord Char
inpToMap xs = M.fromList elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = if char /= '.' then Just ((x,y), char) else Nothing

neighs :: M.Map Coord Char -> Coord -> [Coord]
neighs m c@(x,y) = neighs' m c (m M.! c)

neighs' :: M.Map Coord Char -> Coord -> Char -> [Coord]
neighs' m c@(x,y) char = case char of
                  '.' -> error $ "expected pipe, found ground at " ++ show c
                  'S' -> filter validS [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
                  '|' -> [(x, y-1), (x, y+1)]
                  '-' -> [(x-1, y), (x+1, y)]
                  'L' -> [(x, y-1), (x+1, y)]
                  'J' -> [(x, y-1), (x-1, y)]
                  '7' -> [(x, y+1), (x-1, y)]
                  'F' -> [(x, y+1), (x+1, y)]
    where
        validS n = n `M.member` m && m M.! n /= '.' && c `elem` neighs m n

printMap :: M.Map Coord Char -> String
printMap m = unlines $ map line [0..maxY] where
    line y = map (\x -> fromMaybe '.' (M.lookup (x,y) m)) [0..maxX]
    maxX = maximum $ map (fst . fst) (M.toList m)
    maxY = maximum $ map (snd . fst) (M.toList m)

-- breathe :: M.Map Coord Char -> M.Map Coord Char
breathe m = M.unions $ map (uncurry expand) $ M.toList m where
    base :: Coord -> Char -> M.Map Coord Char
    base (x',y') c = let (x,y) = (3*x'+1, 3*y'+1) in M.fromList [((x,y), c)]

    expand :: Coord -> Char -> M.Map Coord Char
    expand (x,y) '|' = M.union (M.fromList [((3*x+1, 3*y+0), '|'), ((3*x+1, 3*y+2), '|')]) (base (x,y) '|')
    expand (x,y) '-' = M.union (M.fromList [((3*x+0, 3*y+1), '-'), ((3*x+2, 3*y+1), '-')]) (base (x,y) '-')
    expand (x,y) 'L' = M.union (M.fromList [((3*x+1, 3*y+0), '|'), ((3*x+2, 3*y+1), '-')]) (base (x,y) 'L')
    expand (x,y) 'F' = M.union (M.fromList [((3*x+1, 3*y+2), '|'), ((3*x+2, 3*y+1), '-')]) (base (x,y) 'F')
    expand (x,y) 'J' = M.union (M.fromList [((3*x+1, 3*y+0), '|'), ((3*x+0, 3*y+1), '-')]) (base (x,y) 'J')
    expand (x,y) '7' = M.union (M.fromList [((3*x+1, 3*y+2), '|'), ((3*x+0, 3*y+1), '-')]) (base (x,y) '7')
    expand (x,y) 'S' = expand (x,y) (realS (x,y))

    realS :: Coord -> Char
    realS c = let s = sort $ neighs' m c 'S' in head $ filter (\r -> sort (neighs' m c r) == s) "|-LJ7F"



outsideNeighs :: M.Map Coord Char -> Coord -> [Coord]
outsideNeighs m (x,y) = {-let r = -} filter notPipe [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]{- in traceShow ("neighs", (x,y), r) r-} where
    notPipe c@(x,y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY && c `M.notMember` m -- we pass in the expanded . bfs'ed pipe, which only contains the expanded pipe and no ground or other elements
    maxX = 3 + maximum (map (fst . fst) (M.toList m))
    maxY = 3 + maximum (map (snd . fst) (M.toList m))

inside :: M.Map Coord Char -> Int
inside m = sum $ map line [0..maxY] where
    line y = length $ filter (\x -> M.notMember (x,y) m) [0..maxX]
    maxX = maximum $ map (fst . fst) (M.toList m)
    maxY = maximum $ map (snd . fst) (M.toList m)


main :: IO ()
main = do
    input <- lines <$> getContents
    let m = inpToMap input
    let s = fst $ head $ filter (\(c,m) -> m == 'S') (M.toAscList m)
    let search_result = bfs (neighs m) [(s,0)] M.empty

    putStr "part 1: "; print $ maximum $ M.elems search_result

    let loop_only = M.mapWithKey (\c d -> m M.! c) search_result

    let b = breathe loop_only
    putStr $ printMap m
    putStr $ printMap b

    let reachable' = M.keys $ bfs (outsideNeighs b) [((0,0), 0)] M.empty
    let reachable = map (\(x,y) -> (x `div` 3, y `div` 3)) $ filter (\(x,y) -> x `mod` 3 == 1 && y `mod` 3 == 1) reachable'
    print reachable

    let io = M.union (M.fromList $ map (, 'O') reachable) loop_only

--     putStr $ printMap io
    putStr "part 2: "; print $ inside io
