module Day23 where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace (traceShow, traceShowId)
import Data.Bifunctor (Bifunctor(first, second))

type Neighs a = a -> [(a, Int)]
bfs :: (Show k, Ord k) => Neighs k -> [(k, Int, S.Set k)] -> M.Map k Int -> M.Map k Int
bfs f [] ls = ls
bfs f ((q,dist,visited):qs) ls = if new
    then bfs f queue' ls'
    else bfs f qs ls
    where
        new = S.notMember q visited
        neighs = map (\(a,b) -> (a, dist+b, visited')) $ f q
        queue' = neighs ++ qs
        visited' = S.insert q visited
        ls' = M.insertWith max q dist ls

-- bfs1 :: Ord k => Neighs k -> [(k, S.Set k, Int)] -> M.Map k Int -> M.Map k Int
-- bfs1 f [] visited = visited
-- bfs1 f ((q, done, dist):qs) visited = if new
--     then bfs1 f queue' visited'
--     else bfs1 f qs visited
--     where
--         new = M.notMember q visited || (S.notMember q done && visited M.! q <= dist)
--         neighs = f q
--         visited' = M.insertWith min q dist visited
--         done' = S.insert q done
--         queue' = qs ++ map (,done',dist-1) neighs

-- dijkstra :: (Ord k, Show k) => Neighs k -> k -> k -> [Int]
-- dijkstra f target start = search S.empty (S.singleton (0, start)) [] where
--     search visited queue optimum = case S.minView queue of
--         Nothing -> optimum
--         Just ((cost, q), qs)
--             | q == target -> search visited' qs' (cost:optimum)
--             | q /= target && q `S.member` visited -> search visited qs optimum
--             | otherwise -> search visited' qs' optimum where
--                 visited' = S.insert q visited
--                 qs' = foldr (S.insert . (cost-1,)) qs (f q)

-- bfs' :: (Show k, Ord k) => Neighs k -> M.Map k (S.Set k) -> M.Map k Int -> M.Map k Int
-- bfs' f queue ls = if M.null queue
--     then ls
--     else let ((q,visited),qs) = M.deleteFindMin queue in go ((q,visited),qs)
--     where
--         go ((q,visited), qs) = if new
--         then bfs' f queue' ls'
--         else bfs' f qs ls where
--             new = S.notMember q visited
--             neighs = f q
--             queue' = foldr (\k s -> M.insertWith max k visited' s) qs neighs
--             visited' = S.insert q visited
--             ls' = M.insertWith max q (S.size visited) ls


-- bfs' :: (Show k, Ord k, A.Ix k) => Neighs k -> k -> (k, k) -> [k] -> A.Array k Int
-- bfs' f start bounds valids = vals where
--     vals = A.array bounds (map (\c -> (c, go c)) valids)
--     go c = if c == start then 0 else
--         maximum $ 0 : map ((1+) . (vals A.!)) (filter (\i -> i >= fst bounds && i <= snd bounds) (f c))

neighs :: M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
neighs m c@(x,y) = filter isValid base where
    isValid (x,y) = '#' /= fromMaybe '#' (M.lookup (x,y) m)
    base = case fromMaybe '#' $ M.lookup c m of
        '#' -> []
        '.' -> [(x-1, y), (x+1, y), (x, y-1), (x,y+1)]
        '>' -> [(x+1, y)]
        '<' -> [(x-1, y)]
        '^' -> [(x, y-1)]
        'v' -> [(x, y+1)]

-- vroom :: M.Map (Int, Int) Char -> Int -> (Int, Int) -> [((Int, Int), Int)]
-- vroom m i c = case neighs m c of
--     [c'] -> vroom m (i+1) c'
--     xs -> map (,i) xs
vroom :: (M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]) -> M.Map (Int, Int) Char -> (Int, Int) -> [((Int, Int), Int)]
vroom neighs m c = map (\s -> vroomDir s c) (neighs m c) where
    invalid (x,y) = '#' == fromMaybe '#' (M.lookup (x,y) m)
    vroomDir c p = (\xs -> (last xs, length xs)) $ vroomStep c p
    diff (a,b) (x,y) = (x+a, y+b)
    vroomStep cur prev
        | invalid cur = []
        | length succs == 2 && elem prev succs = let next = head (filter (/= prev) succs) in cur : vroomStep next cur
        | otherwise = [cur] where
            succs = neighs m cur



-- vroomify :: M.Map (Int, Int) Char -> M.Map (Int, Int) [((Int, Int), Int)]
-- vroomify orig = vroomN M.empty [(1,0)] where
--     vroom1 m c = if M.member c m then (m, []) else (M.insert c (vroom orig 1 c) m, map fst $ vroom orig 1 c)
--     vroomN m [] = m
--     vroomN m (x:xs) = let (m', v) = vroom1 m x in vroomN m' (v ++ xs)
vroomify :: (M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]) -> M.Map (Int, Int) Char -> M.Map (Int, Int) [((Int, Int), Int)]
vroomify n m = M.mapWithKey (\k v -> if v /= '#' then vroom n m k else []) m


neighs' :: M.Map (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
neighs' m c@(x,y) = filter isValid base where
    isValid (x,y) = '#' /= fromMaybe '#' (M.lookup (x,y) m)
    base = [(x-1, y), (x+1, y), (x, y-1), (x,y+1)]


inpToMap :: [[Char]] -> M.Map (Int, Int) Char
inpToMap xs = M.fromList elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = Just ((x,y), char)

main :: IO ()
main = do
    input <- lines <$> getContents
    let m = inpToMap input
    let v = vroomify neighs' m

    print $ vroom neighs' m (1,0)

    mapM_ print $ filter (not . null . snd) $ M.toList v

    let bounds = ((0,0), (maximum (map fst (M.keys m)), maximum (map snd (M.keys m))))
    let s = (1,0) :: (Int, Int)
    let t = (\(x,y) -> (x-1, y)) $ fst $ head $ M.toDescList m
    print t

    putStrLn "part 1:"
    let b = bfs (map (,1) . neighs m) [(s, 0, S.empty)] M.empty
    -- let b = bfs1 (v M.!) [(s, S.empty, 0)] M.empty
    -- let b = bfs' (neighs m) (M.singleton s S.empty) M.empty
    -- let b = bfs' (neighs m) s bounds (M.keys m)
    -- let d = dijkstra (neighs m) t s 
    -- print b
    print $ head b

    putStrLn "part 2:"
    -- let b = bfs (neighs' m) [(s, S.empty)] M.empty
    -- let b = bfs1 (neighs' m) [(s, S.empty, 0)] M.empty
    -- let b = bfs' (neighs' m) (M.singleton s S.empty) M.empty
    -- print $ head $ M.toDescList b
    -- let d = dijkstra (neighs' m) t s 
    let b' = bfs (v M.!) [(s, 0, S.empty)] M.empty
    print b'
    -- print d
