module Day18 where

import qualified Data.Map as M
import Data.List
import Prelude hiding (Right, Left)
import qualified Prelude
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow)
import Data.Bifunctor (bimap)

-- parsing
data Dir = U | D | R | L deriving (Show, Eq, Ord)
readDir :: String -> Dir
readDir "U" = U
readDir "D" = D
readDir "L" = L
readDir "R" = R

data Cmd = Cmd Dir Integer String deriving (Show, Eq, Ord)
readCmd :: String -> Cmd
readCmd xs = let [d,m,c] = words xs in Cmd (readDir d) (read m) (init $ tail $ tail c)

-- convert part1 command to part2 command
convert :: Cmd -> Cmd
convert (Cmd _ _ color) = Cmd d (read $ "0x" ++ init color) color where
    d = case last color of
        '0' -> R
        '1' -> D
        '2' -> L
        '3' -> U

-- get points and total trench area (not inner area)
dig :: ((Integer, Integer), Integer) -> Cmd -> ((Integer, Integer), Integer)
dig ((x,y), total) (Cmd U i _) = ((x, y-i), total + i)
dig ((x,y), total) (Cmd D i _) = ((x, y+i), total + i)
dig ((x,y), total) (Cmd L i _) = ((x-i, y), total + i)
dig ((x,y), total) (Cmd R i _) = ((x+i, y), total + i)
digs :: [Cmd] -> ([(Integer, Integer)], Integer)
digs cmds = let s = scanl dig ((0,0),0) cmds in (map fst s, snd (last s))

----------- SHOELACE FORMULA FOR POLYGONAL AREA ----------
-- shamelessly stolen from Rosetta Code and slightly adapted to be less magical
shoelace :: [(Integer, Integer)] -> Integer
shoelace xs = (`div` 2) $ abs $ uncurry (-) $ foldr calcSums (0,0) $ zip xs (tail $ cycle xs) where
    calcSums ((x, y), (a, b)) (i,j) = (x * b + i, a * y + j)

main :: IO ()
main = do
    input <- map readCmd . lines <$> getContents

    let (pts, trench) = digs input
    putStr "part 1: "; print $ shoelace pts + (trench `div` 2) + 1

    let (pts, trench) = digs $ map convert input
    putStr "part 2: "; print $ shoelace pts + (trench `div` 2) + 1
