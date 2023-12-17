module Day17 where
import qualified Data.Map as M
import Data.List (insertBy, mapAccumL, unfoldr)
import Data.Function (on)
import Data.Char (intToDigit, digitToInt)
import Data.Maybe (catMaybes)
import qualified Data.Bifunctor
import Debug.Trace (traceShow)

type Neighs a = a -> [a]
bfs' :: (Ord k, Show k) => Neighs (k, Int) -> [(k, Int)] -> M.Map k Int -> M.Map k Int
bfs' f [] visited = visited
bfs' f ((q, dist):qs) visited = -- traceShow ("TRACE", dist) $
    if new
    then traceShow ("TRACE", q, dist) $ bfs' f queue' visited'
    else bfs' f qs visited
    where
        new = M.notMember q visited -- || dist < visited M.! q
        neighs = f (q, dist)
        visited' = M.insertWith min q dist visited
        queue' = foldr (insertBy (compare `on` snd)) neighs qs

data Location = Horizontal (Int, Int) | Vertical (Int, Int) deriving (Show, Eq, Ord)
neighs :: M.Map (Int, Int) Int -> Int -> Int -> (Location, Int) -> [(Location, Int)]
neighs m min max (Horizontal (x,y), cost) = drop (min-1) (unfoldr (go (-)) (cost, [1..max])) ++ drop (min-1) (unfoldr (go (+)) (cost, [1..max])) where
    go op (_, []) = Nothing
    go op (cost, x':xs) = do
        let c = (x `op` x', y)
        l <- (cost +) <$> M.lookup c m
        pure ((Vertical c, l), (l,xs))
neighs m min max (Vertical (x,y), cost) = drop (min-1) (unfoldr (go (-)) (cost, [1..max])) ++ drop (min-1) (unfoldr (go (+)) (cost, [1..max])) where
    go op (_, []) = Nothing
    go op (cost, y':ys) = do
        let c = (x, y `op` y')
        l <- (cost +) <$> M.lookup c m
        pure ((Horizontal c, l), (l,ys))

locToCoord :: Location -> (Int, Int)
locToCoord (Horizontal (x,y)) = (x,y)
locToCoord (Vertical (x,y)) = (x,y)

inpToMap :: [[Char]] -> M.Map (Int, Int) Char
inpToMap xs = M.fromList elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = Just ((x,y), char)

main :: IO ()
main = do
    input <- inpToMap . lines <$> getContents
    let m = M.map digitToInt input
    let src = bfs' (neighs m 1 3) [(Horizontal (0,0), 0), (Vertical (0,0), 0)] M.empty
    -- mapM_ print $ M.toList src
    putStr "part 1: "; print $ M.fromListWith min $ map (Data.Bifunctor.first locToCoord) $ M.toList src
    let src2 = bfs' (neighs m 4 10) [(Horizontal (0,0), 0), (Vertical (0,0), 0)] M.empty
    putStr "part 2: "; print $ M.fromListWith min $ map (Data.Bifunctor.first locToCoord) $ M.toList src2

