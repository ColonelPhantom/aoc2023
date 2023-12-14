module Day14 (main) where
import Data.List (transpose)
import qualified Data.Map as M
import Debug.Trace (traceShow, trace)

-- Repeated rolling isn't very efficient, but good enough
roll1 :: [Char] -> [Char]
roll1 [] = []
roll1 ('.':'O':xs) = 'O' : roll1 ('.':xs)
roll1 (x:xs) = x : roll1 xs

roll :: [Char] -> [Char]
roll xs = if roll1 xs == xs then xs else roll (roll1 xs)

rollNorth, rollSouth, rollWest, rollEast :: [String] -> [String]
rollNorth = transpose . map roll . transpose
rollSouth = transpose . map (reverse . roll . reverse) . transpose
rollWest = map roll
rollEast = map (reverse . roll . reverse)

spin :: [String] -> [String]
spin = rollEast . rollSouth . rollWest . rollNorth

doSpin :: Int -> [String] -> [String]
doSpin i xs = trace ("spinning " ++ show i ++ " times") $ iterate spin xs !! i -- apply spin i times

findSpinCycle :: M.Map [String] Int -> [String] -> (Int, Int, [String])
findSpinCycle xss xs = trace ("trying to find cycle, " ++ show (M.size xss)) $ case M.lookup xs xss of
    Just i -> (i, length xss, xs)
    Nothing -> findSpinCycle (M.insert xs (M.size xss) xss) (spin xs)

score :: [String] -> Int
score = sum . zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

main :: IO ()
main = do
    input <- lines <$> getContents

    putStr "part 1: "
    print $ score $ rollNorth input

    putStr "part 2: "
    let (cycleTo, cycleStart, mid) = findSpinCycle M.empty input
    let cycleSize = cycleStart - cycleTo
    -- let mid = doSpin cycleStart input
    let remainder = (1000000000 - cycleStart) `mod` cycleSize
    print $ score $ doSpin remainder mid