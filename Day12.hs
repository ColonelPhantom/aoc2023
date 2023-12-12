module Day12 where
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Array as A

validsMemo :: [Char] -> [Int] -> Int
validsMemo xss yss = valids xss yss where
    lx = length xss
    ly = length yss
    vals = A.array ((0,0), (length xss, length yss)) -- Maps from dropped chars to actual result
            [((x,y), validsO (drop x xss) (drop y yss)) | x <- [0..lx], y <- [0.. ly]]

    valids :: [Char] -> [Int] -> Int
    valids dx dy = vals A.! (lx - length dx, ly - length dy) -- uses vals to prevent duplicate calculations

    validsO :: [Char] -> [Int] -> Int
    validsO [] [] = 1 -- we found a valid arrangement woo!
    validsO [] _  = 0 -- no text but still some damaged springs
    validsO ('#':_) []  = 0 -- no springs but still some damaged text
    validsO ('.':xs) ys = valids xs ys -- only one option: skip
    -- On #, we drop the head of [Int] as a damaged run.
    -- We check that a damaged run is indeed possible here, otherwise we return 0 as we cannot make it valid
    validsO xs@('#':_) (y:ys) = if length xs >= y -- there have to be at least y springs to be y damaged springs
                                && '.' `notElem` take y xs -- there cannot be undamaged springs in the group
                                && (length xs == y || xs !! y /= '#') -- after the group, we do not have a damaged string
                            then valids (drop (y+1) xs) ys -- drop the group, and the spring after (it must be undamaged)
                            else 0 -- we have to have a damaged group here, but not possible. invalid.
    -- For ?, we both try the '.' case aka just discard and the '#' case too which we call
    -- We call it as validsO and not valids because the latter would yield an infinite loop (the memoized version doesn't know we replaced the char)
    validsO ('?':xs) ys = valids xs ys + validsO ('#':xs) ys

unfold :: (String, [Int]) -> (String, [Int])
unfold (xs, ys) = (intercalate "?" (replicate 5 xs), concat $ replicate 5 ys)

parse :: String -> (String, [Int])
parse xs = let [word, cont] = words xs in
               (word, map read $ splitOn "," cont)

main :: IO ()
main = do
    input <- map parse . lines <$> getContents
    let v = map (uncurry validsMemo) input
    print v
    putStr "part 1: "; print $ sum v

    let v2 = map (uncurry validsMemo . unfold) input
    print v2 -- useful as progress indicator, makes the program feel faster and busier
    putStr "part 2: "; print $ sum v2
