module Day14 (main) where
import Data.List (transpose, intercalate)
import Data.List.Split (splitOn)

roll :: [Char] -> [Char]
roll = intercalate "#" . map (\xs -> filter (== 'O') xs ++ filter (== '.') xs) . splitOn "#"

rollNorth, rollSouth, rollWest, rollEast :: [String] -> [String]
rollNorth = transpose . map roll . transpose
rollSouth = transpose . map (reverse . roll . reverse) . transpose
rollWest = map roll
rollEast = map (reverse . roll . reverse)

spin :: [String] -> [String]
spin = rollEast . rollSouth . rollWest . rollNorth

part2 :: Int -> [String] -> [String]
part2 reps input = spins !! (cycleSize - remainder - 1) where
    (cycleSize, cycleStart, spins) = findRepeat [] input
    remainder = (reps - cycleStart) `mod` cycleSize

    findRepeat :: [[String]] -> [String] -> (Int, Int, [[String]])
    findRepeat xss xs = case lookup xs (zip xss [1..]) of
        Just i -> (i, length xss, xss)
        Nothing -> findRepeat (xs:xss) (spin xs)

score :: [String] -> Int
score = sum . zipWith (*) [1..] . reverse . map (length . filter (== 'O'))

main :: IO ()
main = do
    input <- lines <$> getContents

    putStr "part 1: "
    print $ score $ rollNorth input

    putStr "part 2: "
    print $ score $ part2 1000000000 input