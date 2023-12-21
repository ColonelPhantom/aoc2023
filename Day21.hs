module Day21 where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import System.IO ( hFlush, stdout )
import qualified Data.Array as A
import Data.List (unfoldr)

type Coord = (Int, Int)

-- part 1
succs :: M.Map Coord Char -> Coord -> [Coord]
succs m (x,y) = filter valid [(x-1, y), (x+1, y), (x, y-1), (x,y+1)] where
    valid (x,y) = fromMaybe '#' (M.lookup (x,y) m) /= '#'

step ::  M.Map Coord Char -> S.Set Coord -> S.Set Coord
step m cs = S.unions $ map (S.fromList . succs m) $ S.toList cs

inpToMap :: [[Char]] -> M.Map Coord Char
inpToMap xs = M.fromList elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = Just ((x,y), char)

-- part 2
type GC = (Coord, Coord)
succs' :: M.Map Coord Char -> GC -> [GC]
succs' m = go where
    mx = 1 + maximum (map fst $ M.keys m)
    my = 1 + maximum (map snd $ M.keys m)
    go ((gx,gy), (x,y)) = map toGc $ filter valid [(x-1, y), (x+1, y), (x, y-1), (x,y+1)] where
        valid (x,y) = x < 0 || x >= mx || y < 0 || y >= my || ((m M.! (x,y)) /= '#')
        toGc (x,y) = ((gx + (x `div` mx), gy + (y `div` my)), (x `mod` mx, y `mod` my))

step' ::  M.Map Coord Char -> S.Set GC -> S.Set GC
step' m cs = S.unions $ map (S.fromList . succs' m) $ S.toList cs


succs2 :: M.Map Coord Char -> GC -> S.Set GC
succs2 m = S.fromList . concatMap (succs' m) . succs' m

-- invert :: M.Map a b -> M.Map b a
invert :: (Ord k, Ord a) => M.Map (S.Set a) k -> M.Map k (S.Set a)
invert m = M.fromListWith S.union $ map (\(a,b) -> (b,a)) $ M.toList m

invert2 :: (Ord a, Ord b) => M.Map (S.Set a) (S.Set b) -> M.Map (S.Set a) (S.Set b)
invert2 = invert . invert

type State = M.Map (S.Set Coord) (S.Set Coord)
-- step' :: M.Map Coord Char -> M.Map (S.Set Coord) (S.Set Coord) -> M.Map (S.Set Coord) (S.Set Coord)
-- step' m cs = M.unionsWith S.union $ map (M.fromList . filter (not . S.null . snd)) [lsuccs,rsuccs,usuccs,dsuccs] where
--     -- xs = M.unionsWith S.union $ map (M.fromList . filter (not . S.null . snd)) [succs,lsuccs,rsuccs,usuccs,dsuccs]

--     groups = M.fromList $ concatMap (\gcs -> map (, gcs) (S.toList gcs)) $ M.keys succs

--     foreigns = concatMap (\(g,l) -> map (,l) (S.toList g)) $ concat [lsuccs, rsuccs, usuccs, dsuccs]
--     succs = M.map (S.fromList . concatMap (map snd . filter (\((gx,gy),_) -> gx == 0 && gy == 0) . succs' m) . S.toList) cs
--     lsuccs = map (\(gc, lc) -> (S.map (\(gx,gy) -> (gx-1, gy)) gc, S.fromList $ concatMap (map snd . filter (\((gx,gy),_) -> gx == (-1) && gy == 0) . succs' m) (S.toList lc))) $ M.toList cs
--     rsuccs = map (\(gc, lc) -> (S.map (\(gx,gy) -> (gx+1, gy)) gc, S.fromList $ concatMap (map snd . filter (\((gx,gy),_) -> gx ==   1  && gy == 0) . succs' m) (S.toList lc))) $ M.toList cs
--     usuccs = map (\(gc, lc) -> (S.map (\(gx,gy) -> (gx, gy-1)) gc, S.fromList $ concatMap (map snd . filter (\((gx,gy),_) -> gx == 0 && gy == (-1)) . succs' m) (S.toList lc))) $ M.toList cs
--     dsuccs = map (\(gc, lc) -> (S.map (\(gx,gy) -> (gx, gy+1)) gc, S.fromList $ concatMap (map snd . filter (\((gx,gy),_) -> gx == 0 && gy ==   1 ) . succs' m) (S.toList lc))) $ M.toList cs

ssize :: State -> Integer
ssize m = sum $ map (\(gc,lc) -> fromIntegral (S.size gc * S.size lc)) (M.toList m)

main :: IO ()
main = do
    input <- inpToMap . lines <$> getContents
    let s = fst $ head $ filter ((== 'S') . snd) $ M.toList input
    -- print s
    -- print input
    -- print $ succs input s

    let is = iterate (step input) (S.singleton s)
    putStrLn "part 1: "
    putStr "demo: "; print $ length (is !! 6)
    putStr "real: "; print $ length (is !! 64)

    let is = iterate (step' input) (S.singleton ((0,0),s))
    putStrLn "part 2: "
    -- mapM_ (\x -> print x >> hFlush stdout) $ zipWith3 (\i x y -> (i, length y - length x)) [1..] is (tail is)

    -- let idelta = zipWith (\x y -> length x - length y) (tail is) is
    -- let iss = zip [0..] $ zip (1 : take 130 idelta) (take 131 (drop 130 idelta))

    -- print iss
    mapM_ (\x -> print x >> hFlush stdout) $ zip [0..] (1 : map length is)


    -- let p2 = iterate (step' input) (M.singleton (S.singleton (0,0)) (S.singleton s))
    -- putStrLn "part 2: "
    -- mapM_ print $ M.toList (invert2 $ p2 !! 50)

    -- putStr "6 steps: \t"; print $ ssize (p2 !! 6)
    -- putStr "10 steps: \t"; print $ ssize (p2 !! 10)
    -- putStr "50 steps: \t"; print $ ssize (p2 !! 50)
    -- putStr "100 steps: \t"; print $ ssize (p2 !! 100)
    -- putStr "500 steps: \t"; print $ ssize (p2 !! 500)
    -- putStr "1000 steps: \t"; print $ ssize (p2 !! 1000)
    -- putStr "5000 steps: \t"; print $ ssize (p2 !! 5000)


    -- putStr "part2: "; print $ length (is !! 26501365)
    putStrLn "goodbye"
