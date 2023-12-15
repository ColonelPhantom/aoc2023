module Day15 where
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.List.Split (splitOn)

-- part 1: hashing
hashStep :: Int -> Char -> Int
hashStep value char = ((value + ord char) * 17) `mod` 256

hash :: String -> Int
hash = foldl hashStep 0

-- command parsing for part 2
data Cmd = Remove String | Insert String Int

parseCmd :: [Char] -> Cmd
parseCmd cmd = if '=' `elem` cmd
    then let [label, num] = splitOn "=" cmd in Insert label (read num)
    else Remove (init cmd)

-- part 2
type Box = [(String, Int)]

step :: M.Map Int Box -> Cmd -> M.Map Int Box
step xs (Remove label) = M.adjust (filter (\(l, s) -> l /= label)) (hash label) xs
step xs (Insert label strength) = M.alter (Just . ins) (hash label) xs where
    ins Nothing = [(label, strength)]
    ins (Just xs) = if label `elem` map fst xs
        then map (\(l, s) -> if l == label then (l, strength) else (l,s)) xs
        else xs ++ [(label, strength)]

power :: M.Map Int Box -> Int
power m = sum $ map (\(i, box) -> (i+1) * pbox box) $ M.toList m where
    pbox :: Box -> Int
    pbox box = sum $ zipWith (*) [1..] $ map snd box

-- entry point
main :: IO ()
main = do
    input <- splitOn "," <$> getContents
    putStr "part 1: "; print $ sum $ map hash input

    let cmds = map parseCmd input
    putStr "part 2: "; print $ power $ foldl step M.empty cmds