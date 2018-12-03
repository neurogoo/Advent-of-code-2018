{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import           Data.Char
import           Data.List

import qualified Data.Set  as S

day2_1 :: IO Int
day2_1 = do
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day2.txt"
    let exactlyTwice = length $ filter (\x -> any (\y -> length y == 2) x) $ ((group . sort) <$> lines input)
        exactlyThrice = length $ filter (\x -> any (\y -> length y == 3) x) $ ((group . sort) <$> lines input)
    pure $ exactlyTwice * exactlyThrice

day2_2 :: IO String
day2_2 = do
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day2.txt"
    let i = lines input
        wordSize = length $ head i
        sameness x y = filter (/= ' ') $ zipWith (\a b -> if a == b then a else ' ') x y
        similar = concat $ filter (not . null) $ fmap (\x -> filter (\y -> wordSize - 1 == length (sameness x y)) i) i
    pure $ sameness (head similar) (head $ tail $ similar)
