module Day02 where

import Data.List (dropWhile)     -- used for removing "Game x:"
import Data.List.Split (splitOn) -- extremely useful, splits input on delimiter

maxAll :: Ord a => (a,a,a) -> (a,a,a) -> (a,a,a)
maxAll (a,b,c) (x,y,z) = (max a x, max b y, max c z)

parseGame :: String -> [(Int, Int, Int)]
parseGame = map readRevelation . revelations where
    revelations = splitOn ";" . tail . dropWhile (/= ':') -- strip off "Game x:" and split on ;

    readRevelation = readReveal . map readCube . cubes where
        -- cubes splits " 3 blue, 4 red" into [["3","blue"], ["4", "red"]]
        -- readCube uses read to parse this into tuples of (Int, String) like (3,"blue") and (4,"red")
        -- readReveal combines these tuples into actual cube amounts in tuples like (4,0,3)
        cubes       = map (tail . splitOn " ") . splitOn ","
        readCube [num, color] = (read num :: Int, color)
        readReveal = foldl updateTuple (0,0,0) where
            updateTuple (r,g,b) (new, "red")   = (new, g, b)
            updateTuple (r,g,b) (new, "green") = (r, new, b)
            updateTuple (r,g,b) (new, "blue")  = (r, g, new)

part1 :: [[(Int, Int, Int)]] -> Int
part1 = sum . map fst . filter valid . map maxes . ids where
    ids = zip [1..]                                    -- remember game IDs
    maxes (i, game) = (i, foldr maxAll (0, 0, 0) game) -- determine required cubes per game
    valid (i, (r,g,b)) = r <= 12 && g <= 13 && b <= 14 -- check if possible with provided cubes

part2 :: [[(Int, Int, Int)]] -> Int
part2 = sum . map (power . maxes) where
    maxes = foldr maxAll (0,0,0) -- determine required cubes
    power (r,g,b) = r*g*b        -- power of cubes as specified

main :: IO ()
main = do
    games <- map parseGame . lines <$> getContents

    putStr "part 1: "; print $ part1 games
    putStr "part 2: "; print $ part2 games
