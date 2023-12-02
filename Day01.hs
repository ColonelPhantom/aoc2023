module Day01 where
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- mapt is like map, but passes the entire remainder of the list to the function
-- whereas map only passes the current element
mapt :: ([a] -> b) -> [a] -> [b]
mapt f [] = []
mapt f list@(x:xs) = f list : mapt f xs

substs :: [(String, Char)]
substs = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'),
          ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

replace :: String -> String
-- try all substitions in all positions
replace = mapt (\string -> head $ foldr applySubst string substs) where
    -- applySubst only substitutes the first character, so "one" becomes "1ne";
    -- required to interpret "oneight" as 18 correctly
    applySubst (from,to) orig@(x:xs) = if from `isPrefixOf` orig then to:xs else orig

readDigits :: String -> Int
readDigits digits = read [head digits, last digits]

main = do
    input <- lines <$> getContents

    putStr "part 1: "
    print $ sum $ map (readDigits . filter isDigit) input

    putStr "part 2: "
    print $ sum $ map (readDigits . filter isDigit . replace) input
