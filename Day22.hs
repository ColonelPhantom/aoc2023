module Day22 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.List (delete)


type Coord = (Int, Int, Int)
type Brick = (Coord, Coord)

type VBrick = (Int, Int)

type World = M.Map (Int, Int) (S.Set VBrick)

fall :: Brick -> World -> (Brick, World)
fall ((bfx,bfy,bfz), (btx,bty,btz)) world = (newBrick, newWorld) where
    coords = [(x,y) | x <- [bfx .. btx], y <- [bfy..bty]]
    fallxy (x,y) = 1 + maybe 0 snd (S.lookupLT (bfz,btz) (world M.! (x,y)))
    fallAmount = bfz - maximum (map fallxy coords)
    newBrick = ((bfx,bfy,bfz-fallAmount), (btx,bty,btz-fallAmount))
    updatexy = S.insert (bfz-fallAmount, btz-fallAmount) . S.delete (bfz,btz)
    newWorld = foldr (M.adjust updatexy) world coords

fallAll :: [Brick] -> World -> ([Brick], World)
fallAll [] w = ([], w)
fallAll (x:xs) w = do
    let (b,w') = fall x w
    let (bs,w'') = fallAll xs w'
    (b:bs, w'')

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

conv :: Eq c => (c -> c) -> c -> c
conv x = converge (==) . iterate x

fallConv :: [Brick] -> ([Brick], World)
fallConv bs = conv (uncurry fallAll) (bs, initWorld bs)

initWorld :: [Brick] -> World
initWorld = foldr insertBrick M.empty where
    insertBrick :: Brick -> World -> World
    insertBrick ((bfx,bfy,bfz), (btx,bty,btz)) m = foldr (M.alter (Just . ins)) m coords where
        ins Nothing = S.singleton (bfz, btz)
        ins (Just s) = S.insert (bfz, btz) s
        coords = [(x,y) | x <- [bfx .. btx], y <- [bfy..bty]]

canDisintegrate :: [Brick] -> World -> Brick -> Bool
canDisintegrate bs w b@((bfx,bfy,bfz), (btx,bty,btz)) = traceShow ("checking", b) fallb == b'
    where
        coords = [(x,y) | x <- [bfx .. btx], y <- [bfy..bty]]
        removexy = S.delete (bfz, btz)
        b' = delete b bs
        w' = foldr (M.adjust (S.delete (bfz, btz))) w coords
        (fallb, fallw) = fallAll (delete b bs) w'

canDisintegrateX :: [Brick] -> World -> Brick -> Int
canDisintegrateX bs w b@((bfx,bfy,bfz), (btx,bty,btz)) = traceShow ("diffing", b)
    (diffs fallb b')
    where
        coords = [(x,y) | x <- [bfx .. btx], y <- [bfy..bty]]
        removexy = S.delete (bfz, btz)
        b' = delete b bs
        w' = foldr (M.adjust (S.delete (bfz, btz))) w coords
        (fallb, fallw) = conv (uncurry fallAll) (b', w')
        diffs xs ys = sum $ zipWith (\x y -> if x /= y then 1 else 0) xs ys


readCoord :: String -> (Int, Int, Int)
readCoord xs = read ("(" ++ xs ++ ")")
readBrick :: [Char] -> ((Int, Int, Int), (Int, Int, Int))
readBrick xs = let [f,t] = splitOn "~" xs in (readCoord f, readCoord t)

main :: IO ()
main = do
    input <- map readBrick . lines <$> getContents
    -- print $ maximum $ map (\(mi,(mx,my,mz)) -> mx) input
    let (bs,w) = fallConv input
    putStr "part 1: "; print $ length $ filter (canDisintegrate bs w) bs
    putStrLn "part 2: "; print $ sum $ map (canDisintegrateX bs w) bs