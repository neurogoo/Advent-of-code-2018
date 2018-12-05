module Day5 where

import           Data.Char
import           Data.List

safeInit x = if x == "" then "" else init x
safeLast x = if x == "" then "" else [last x]
shouldReact c1 c2 = toLower c1 == toLower c2 && c1 /= c2

processPolymer :: String -> String -> String
processPolymer s (x:y:rest) | shouldReact x y = processPolymer (safeInit s) (safeLast s ++ rest)
                            | otherwise = processPolymer (s ++ [x]) (y:rest)
processPolymer s rest = s ++ rest

day5_1 :: IO Int
day5_1 = do
    input <- init <$> readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day5.txt"
    pure $ length $ processPolymer "" input

day5_2 :: IO Int
day5_2 = do
    input <- init <$> readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day5.txt"
    let allLetters = nub $ toLower <$> input
        processed = minimum $ fmap (\l -> length $ processPolymer "" ((filter (/= l) . filter (/= toUpper l)) input)) allLetters
    pure processed
