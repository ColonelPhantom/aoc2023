module Day20 where

import qualified Data.Map as M
import Data.List.Split (splitOn)

data PulseT = High | Low deriving (Show, Eq, Ord)
type Pulse = (PulseT, String, String)
data ModuleT = Broad | FF Bool | Conj (M.Map String PulseT) | Rx Bool deriving Show
type Module = (ModuleT, [String])
type State = M.Map String Module

readMod :: String -> (String, Module)
readMod xs = case id of
    "broadcaster" -> ("broadcaster", (Broad, tos))
    ('%':n) -> (n, (FF False, tos))
    ('&':n) -> (n, (Conj M.empty, tos))
    _ -> error $ "unexpected mod id " ++ show id
    where
        [id, tos1] = splitOn " -> " xs
        tos = splitOn ", " tos1

readMods :: [String] -> State
readMods xss = M.insert "rx" (Rx False, []) inited where
    xs = map readMod xss
    inited = foldr (\(f,t) m -> insertMod m f t) (M.fromList xs) xs
    insertMod :: State -> String -> Module -> State
    insertMod m from (_, tos) = foldr (\t m -> go m (from, t)) m tos
    go :: State -> (String, String) -> State
    go m (from, to) = case M.lookup to m of
        Just (Conj s, o) -> M.insert to (Conj (M.insert from Low s), o) m
        _ -> m


processPulse :: Pulse -> State -> (State, [Pulse])
processPulse (typ, from, to) st = case (typ, modul) of
    (_, Nothing) -> (st, [])
    (_, Just Broad) -> (st, map (Low,to,) outs)
    (Low, Just (Rx _)) -> (M.insert to (Rx True, outs) st, [])
    (High, Just (Rx _)) -> (st, [])
    (High, Just (FF _)) -> (st, []) -- ff ignores high
    (Low, Just (FF False)) -> (M.insert to (FF True,  outs) st, map (High,to,) outs)
    (Low, Just (FF True )) -> (M.insert to (FF False, outs) st, map (Low ,to,) outs)
    (p, Just (Conj m)) -> let m' = M.insert from p m
                              p' = if all (== High) (M.elems m') then Low else High in
                (M.insert to (Conj m', outs) st, map (p',to,) outs)
    where
        (modul, outs) = case M.lookup to st of
            Just (m,o) -> (Just m,o)
            Nothing -> (Nothing, [])

processPulses :: [Pulse] -> State -> (State, [Pulse])
processPulses [] m = (m, [])
processPulses (p:ps) m = let (m', ps') = processPulse p m
                             (m'', ps'') = processPulses (ps++ps') m' in
                                (m'', p:ps'')

broadcast :: Pulse
broadcast = (Low, "button", "broadcaster")

pushButton :: State -> (State, [Pulse])
pushButton = processPulses [broadcast]

pushButtonN :: Int -> State -> (State, [Pulse])
pushButtonN 0 m = (m, [])
pushButtonN n m = let (m',p') = pushButton m
                      (m'', p'') = pushButtonN (n-1) m' in
                        (m'', p' ++ p'')

pushButtonRx :: State -> Int
pushButtonRx s = let (s', _) = pushButton s in
    case fst $ s' M.! "rx" of
        (Rx True) -> 1
        (Rx False) -> 1 + pushButtonRx s'

main :: IO ()
main = do
    input <- readMods . lines <$> getContents
    print input
    let (_s', ps) = pushButtonN 1000 input
    -- mapM_ print ps
    let highs = length $ filter (\(x,_,_) -> x == High) ps
    let lows = length $ filter (\(x,_,_) -> x == Low) ps
    print (lows, highs)
    putStr "part 1: "; print (lows*highs)
    putStr "part 2: "; print $ pushButtonRx input