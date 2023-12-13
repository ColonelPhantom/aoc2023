module Day13 where

import Control.Applicative (Alternative ((<|>)))
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

reflections :: [String] -> [(Int, [String], [String])]
reflections xs =
  filter (\(i, is, ts) -> and (zipWith (==) (reverse is) ts)) $
    zip3 [1 ..] (init $ tail $ inits xs) (init $ tail $ tails xs)

smudges :: [String] -> [(Int, [String], [String])]
smudges xs =
  filter (\(i, is, ts) -> 1 == sum (zipWith diff (reverse is) ts)) $
    zip3 [0 ..] (inits xs) (tails xs)
  where
    diff x y = length $ filter id $ zipWith (/=) x y

findMirror :: ([String] -> [(Int, [String], [String])]) -> [String] -> Int
findMirror f xs = fromJust (rows <|> cols) where
    rows = (* 100) . fst3 . fst <$> uncons (f xs)
    cols = fst3 . fst <$> uncons (f (transpose xs))

main :: IO ()
main = do
  input <- splitOn [""] . lines <$> getContents
  putStr "part 1: "; print $ sum $ map (findMirror reflections) input
  putStr "part 2: "; print $ sum $ map (findMirror smudges) input