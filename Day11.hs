module Day11 where
import Data.List (transpose)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Array as A

-- find all coords of non-dots
galaxies :: [[Char]] -> [(Int, Int)]
galaxies xs = elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = if char /= '.' then Just (x,y) else Nothing

-- calculate distance between two galaxies
manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (x,y) = abs (x-a) + abs (y-b)

-- part 2 (and now also part 1):
-- use prefix sum to get 'real' coordinates based on map sizes
expand :: Int -> [[Char]] -> A.Array Int Int
expand step xs = A.listArray (0, length xs) $  scanl1 (+) $ map size xs where
    size line = if all (== '.') line then step else 1

solve :: Int -> [[Char]] -> Int
solve step input = do
    let ys = expand step input
    let xs = expand step (transpose input)
    let g = map (\(x,y) -> (xs A.! x, ys A.! y)) $ galaxies input
    let dists = [manhattan a b | (i, a) <- zip [1..] g, (j,b) <- zip [1..] g, i < j]
    sum dists

main :: IO ()
main = do
    input <- lines <$> getContents
    putStr "part 1: "; print $ solve 2 input
    putStr "part 2: "; print $ solve 1000000 input
