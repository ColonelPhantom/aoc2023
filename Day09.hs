module Day09 where

-- part 1
diffs :: [Integer] -> [Integer]
diffs xs = zipWith (-) (tail xs) xs

extendLine :: [Integer] -> Integer
extendLine xs
  | all (==0) xs = 0
  | otherwise = last xs + extendLine (diffs xs)

-- part 2
prependLine :: [Integer] -> Integer
prependLine = extendLine . reverse

-- parsing and main
oasisLine :: String -> [Integer]
oasisLine = map read . words

main :: IO ()
main = do
    lines <- map oasisLine . lines <$> getContents

    putStr "part 1: "; print $ sum $ map extendLine lines
    putStr "part 2: "; print $ sum $ map prependLine lines
