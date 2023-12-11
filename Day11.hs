module Day11 where
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace (traceShow)

empty :: [Char] -> Bool
empty = not . all (== '.')

-- part 1 simple expand
expand :: [[Char]] -> [[Char]]
expand = concatMap go where
    go xs = if empty xs then [xs] else [xs,xs]

expand' = transpose . expand . transpose

-- find all galaxies
galaxies :: [[Char]] -> [(Integer, Integer)]
galaxies xs = elems where
    elems = catMaybes $ concat $ zipWith linep [0..] xs
    linep y = zipWith (`entry` y) [0..]
    entry x y char = if char /= '.' then Just (x,y) else Nothing

manhattan :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhattan (a,b) (x,y) = abs (x-a) + abs (y-b)

-- part 2:
-- use prefix sum to get 'real' coordinates based on map sizes
expand2 :: Integer -> [[Char]] -> M.Map Integer Integer
expand2 step xs = M.fromList $ zip [0..] $ scanl1 (+) $ map go xs where
    go :: [Char] -> Integer
    go line = if empty line then 1 else step

main :: IO ()
main = do
    input <- lines <$> getContents

    let e = expand $ expand' $ input
    let g = galaxies e
    let dists = [manhattan a b | (i, a) <- zip [1..] g, (j,b) <- zip [1..] g, i < j]

    putStr "part 1: "
    print $ sum dists

    putStr "part 2: "
    let step = 1000000
    let ys = expand2 step input
    let xs = expand2 step (transpose input)
    let g = galaxies input
    let mapCoord (x,y) = (xs M.! x, ys M.! y)
    let dists = [manhattan (mapCoord a) (mapCoord b) | (i, a) <- zip [1..] g, (j,b) <- zip [1..] g, i < j]

    print $ sum dists
