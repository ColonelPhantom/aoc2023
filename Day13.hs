module Day13 where

import Data.List
import Data.List.Split (splitOn)
import Control.Applicative (Alternative((<|>)))
import Data.Maybe (fromJust)

fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a

findReflections :: [String] -> [(Int, [String], [String])]
findReflections xs =
  filter (\(i, is, ts) -> not (null is) && not (null ts)) $
    filter (\(i, is, ts) -> and (zipWith (==) (reverse is) ts)) $
      zip3 [0 ..] (inits xs) (tails xs)

findMirror :: ([String] -> [(Int, [String], [String])]) -> [String] -> Int
findMirror f xs = fromJust (rows <|> cols) where
    rows = (*100) . fst3 . fst <$> uncons (f xs)
    cols = fst3 . fst <$> uncons (f (transpose xs))

findSmudges :: [String] -> [(Int, [String], [String])]
findSmudges xs =
    filter (\(i, is, ts) -> 1 == sum (zipWith diff (reverse is) ts)) $
      zip3 [0 ..] (inits xs) (tails xs) where
  diff x y = length $ filter id $ zipWith (/=) x y


main :: IO ()
main = do
  input <- splitOn [""] . lines <$> getContents
  putStr "part 1: "; print $ sum $ map (findMirror findReflections) input
  putStr "part 2: "; print $ sum $ map (findMirror findSmudges) input