module Day19 where
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Cond = String :<: Int | String :>: Int deriving (Show, Eq)
data Rule = If Cond String | Always String deriving (Show, Eq)
type Flows = M.Map String [Rule]
type Part = M.Map String Int

-- parsing
readCond :: String -> Cond
readCond (i:'<':n) = [i] :<: read n
readCond (i:'>':n) = [i] :>: read n

readRule :: String -> Rule
readRule xs = case splitOn ":" xs of
    [id] -> Always id
    [cond, target] -> If (readCond cond) target

readFlow :: String -> (String, [Rule])
readFlow xs = (id,rules) where
    [id, r] = splitOn "{" xs
    r' = init r
    rs = splitOn "," r'
    rules = map readRule rs

readPart :: String -> Part
readPart xs = M.fromList vals where
    xmas = splitOn "," $ init (tail xs)
    vals = zip (map (: []) "xmas") (map (read . drop 2) xmas)

-- part 1
evalCond :: Cond -> Part -> Bool
evalCond (s :<: i) p = (p M.! s) < i
evalCond (s :>: i) p = (p M.! s) > i

evalRules :: [Rule] -> Part -> String
evalRules [] _ = error "invalid rules"
evalRules ((Always s):xs) _ = s
evalRules ((If c s):xs) part = if evalCond c part then s else evalRules xs part

evalFlow :: Flows -> Part -> String -> Bool
evalFlow fs part location = case evalRules (fs M.! location) part of
    "A" -> True
    "R" -> False
    l' -> evalFlow fs part l'

evalFlows :: Flows -> Part -> Bool
evalFlows fs part = evalFlow fs part "in"

score :: M.Map k Int -> Int
score = sum . M.elems

-- part 2
type Parts = M.Map String [PartRange]
type PartRange = M.Map String (Int, Int)

symbRule :: Rule -> PartRange -> (Maybe PartRange, Parts)
symbRule (Always xs) parts = (Nothing, M.singleton xs [parts])
symbRule (If (c :<: i) xs) parts
    | b < i = (Nothing, M.singleton xs [parts]) -- true for all parts
    | i < a = (Just parts, M.empty) -- false for all parts
    | a < i && i < b = (Just excl, M.singleton xs [incl])-- split down the middle
    where
        (a,b) = parts M.! c
        incl = M.insert c (a, i-1) parts
        excl = M.insert c (i, b) parts
symbRule (If (c :>: i) xs) parts
    | a > i = (Nothing, M.singleton xs [parts]) -- true for all parts
    | i > b = (Just parts, M.empty) -- false for all parts
    | a < i && i < b = (Just excl, M.singleton xs [incl])-- split down the middle
    where
        (a,b) = parts M.! c
        incl = M.insert c (i+1, b) parts
        excl = M.insert c (a, i) parts

symbRules :: [Rule] -> PartRange -> Parts
symbRules [] parts = error "invalid rules"
symbRules (r:rs) parts = let (remainder, m) = symbRule r parts in
    case remainder of
        Nothing -> m
        Just r -> M.unionWith (++) m (symbRules rs r)

symbFlows :: Flows -> Parts -> ([PartRange], [PartRange])
symbFlows flows parts = if M.null rest then (a,r) else (a++as, r++rs) where
    (a,r) = (fromMaybe [] (M.lookup "A" parts), fromMaybe [] (M.lookup "R" parts))
    (as, rs) = symbFlows flows remainder
    rest = M.delete "A" $ M.delete "R" parts
    symbParts id parts = M.unionsWith (++) $ map (symbRules (flows M.! id)) parts
    remainder = M.unionsWith (++) $ map (uncurry symbParts) (M.toList rest)

count :: [(Int, Int)] -> Int
count [] = 1
count ((a,b):xs) = (b-a+1) * count xs


main :: IO ()
main = do
    [inputr, inputp] <- splitOn [""] . lines <$> getContents
    let flows = M.fromList $ map readFlow inputr
    let parts = map readPart inputp
    
    putStr "part 1: ";
    let accepted = filter (evalFlows flows) parts
    print $ sum $ map score accepted

    putStr "part 2: "
    let inits = M.singleton "in" [M.fromList $ map (\x -> ([x], (1,4000))) "xmas"]
    let (a,r) = symbFlows flows inits
    print $ sum $ map (count . M.elems) a
