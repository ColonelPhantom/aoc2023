module Day16 where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude hiding (Left, Right)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShow, traceShowId)

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

data Beam = Up Int Int | Down Int Int | Left Int Int | Right Int Int deriving (Show, Eq, Ord)
neighs :: M.Map (Int, Int) Char -> Neighs Beam
neighs m (Up x y) = let dir = Up; x' = x; y' = (y-1) in case M.lookup (x', y') m of
    Nothing -> []
    Just '.' -> [dir x' y']
    Just '|' -> [dir x' y']
    Just '/' -> [Right x' y']
    Just '\\' -> [Left x' y']
    Just '-' -> [Left x' y', Right x' y']
neighs m (Down x y) = let dir = Down; x' = x; y' = y+1 in case M.lookup (x', y') m of
    Nothing -> []
    Just '.' -> [dir x' y']
    Just '|' -> [dir x' y']
    Just '/' -> [Left x' y']
    Just '\\' -> [Right x' y']
    Just '-' -> [Left x' y', Right x' y']
neighs m (Left x y) = let dir = Left; x' = x-1; y' = y in case M.lookup (x', y') m of
    Nothing -> []
    Just '.' -> [dir x' y']
    Just '-' -> [dir x' y']
    Just '/' -> [Down x' y']
    Just '\\' -> [Up x' y']
    Just '|' -> [Up x' y', Down x' y']
neighs m (Right x y) = let dir = Right; x' = x+1; y' = y in case M.lookup (x', y') m of
    Nothing -> []
    Just '.' -> [dir x' y']
    Just '-' -> [dir x' y']
    Just '/' -> [Up x' y']
    Just '\\' -> [Down x' y']
    Just '|' -> [Up x' y', Down x' y']

coords :: Beam -> (Int, Int)
coords (Up x y) = (x,y)
coords (Down x y) = (x,y)
coords (Left x y) = (x,y)
coords (Right x y) = (x,y)

inpToMap :: [[Char]] -> M.Map (Int, Int) Char
inpToMap xs = M.fromList elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = Just ((x,y), char)

-- part 2
startBeams :: M.Map (Int, Int) Char -> [Beam]
startBeams m = concatMap beams (M.keys m) where
    mx = maximum $ map fst $ M.keys m
    my = maximum $ map snd $ M.keys m
    beams (x, y)
      | x == 0 && y == 0 = [Right (x-1) y, Down x (y-1)]
      | x == mx && y == 0 = [Left (x+1) y, Down x (y-1)]
      | x == 0 && y == my = [Right (x-1) y, Up x (y+1)]
      | x == mx && y == my = [Left (x+1) y, Up x (y+1)]
      | x == 0 = [Right (x-1) y]
      | x == mx = [Left (x+1) y]
      | y == 0 = [Down x (y-1)]
      | y == my = [Up x (y+1)]
      | otherwise = []

findUnvisited :: [Beam] -> M.Map Beam Int -> [Beam]
findUnvisited b s = filter (`M.notMember` s) b

part2 :: M.Map (Int, Int) Char -> [Beam] -> Int
part2 m [] = 0
part2 m (b:bs) = do
    let result = bfs (neighs m) (map (,0) (neighs m b)) M.empty
    let count = length $ S.fromList $ map coords $ M.keys result
    max count (part2 m (traceShow (length bs) bs))

main :: IO ()
main = do
    input <- lines <$> getContents
    let m = inpToMap input
    let result = bfs (neighs m) (map (,0) (neighs m (Right (-1) 0))) M.empty
    let visited = S.fromList $ map coords $ M.keys result
    putStr "part 1: "; print $ length visited

    putStr "part 2: "; print $ part2 m (startBeams m)
