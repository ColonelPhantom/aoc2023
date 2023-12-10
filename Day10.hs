module Day10 where

import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)

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
neighs m c@(x,y) = case m M.! c of
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

main :: IO ()
main = do
    input <- lines <$> getContents
    let m = inpToMap input
    let s = fst $ head $ filter (\(c,m) -> m == 'S') (M.toAscList m)
    let search_result = bfs (neighs m) [(s,0)] M.empty

    putStr "part 1: "; print $ maximum $ M.elems search_result

    let loop_only = M.mapWithKey (\c d -> m M.! c) search_result
    putStrLn $ printMap loop_only

    putStrLn "part 2: "
