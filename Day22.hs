module Day22 where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)
import Data.List (delete, mapAccumL, sortOn)

type Coord = (Int, Int, Int)
type Brick = (Coord, Coord)
type World = M.Map (Int, Int) (S.Set (Int, Int))

fall :: World -> Brick -> (World, Brick)
fall world ((bfx,bfy,bfz), (btx,bty,btz)) = (newWorld, newBrick) where
    coords = [(x,y) | x <- [bfx .. btx], y <- [bfy..bty]]
    fallxy (x,y) = 1 + maybe 0 snd (S.lookupLT (bfz,btz) (world M.! (x,y)))
    fallAmount = bfz - maximum (map fallxy coords)
    newBrick = ((bfx,bfy,bfz-fallAmount), (btx,bty,btz-fallAmount))
    updatexy = S.insert (bfz-fallAmount, btz-fallAmount) . S.delete (bfz,btz)
    newWorld = foldr (M.adjust updatexy) world coords

fallAll :: World -> [Brick] -> (World, [Brick])
fallAll = mapAccumL fall

conv :: Eq c => (c -> c) -> c -> c
conv f x = let x' = f x in if x == x' then x else conv f x'

fallConv :: [Brick] -> (World, [Brick])
fallConv bs = conv (uncurry fallAll) (initWorld bs, bs)

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
        b' = delete b bs
        w' = foldr (M.adjust (S.delete (bfz, btz))) w coords
        (fallw, fallb) = fallAll w' b'

canDisintegrateX :: [Brick] -> World -> Brick -> Int
canDisintegrateX bs w b@((bfx,bfy,bfz), (btx,bty,btz)) = traceShow ("diffing", b) (diffs fallb b')
    where
        coords = [(x,y) | x <- [bfx .. btx], y <- [bfy..bty]]
        b' = delete b bs
        w' = foldr (M.adjust (S.delete (bfz, btz))) w coords
        (fallw, fallb) = conv (uncurry fallAll) (w', b')
        diffs xs ys = sum $ zipWith (\x y -> if x /= y then 1 else 0) xs ys

readCoord :: String -> (Int, Int, Int)
readCoord xs = read ("(" ++ xs ++ ")")
readBrick :: [Char] -> ((Int, Int, Int), (Int, Int, Int))
readBrick xs = let [f,t] = splitOn "~" xs in (readCoord f, readCoord t)

main :: IO ()
main = do
    input <- sortOn (\(_, (x,y,z)) -> z) . map readBrick . lines <$> getContents
    let (w, bs) = fallConv input
    putStr "part 1: "; print $ length $ filter (canDisintegrate bs w) bs
    putStrLn "part 2: "; print $ sum $ map (canDisintegrateX bs w) bs