{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import           Control.Monad.ST
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Char

import qualified Data.Set          as S

day1_1 :: IO Integer
day1_1 = do
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day1.txt"
    let changes = ((read :: String -> Integer) . filter (/= '+')) <$> (lines input)
    pure $ sum changes

day1_2 :: IO Integer
day1_2 = do
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day1.txt"
    let changes = ((read :: String -> Integer) . (filter (/= '+'))) <$> (lines input)
    pure $ findDuplicate (S.singleton 0) (cycle changes) 0
  where
    findDuplicate :: S.Set Integer -> [Integer] -> Integer -> Integer
    findDuplicate s (l:ls) sumFreq =
        let newSumFreq = sumFreq + l
        in if S.member newSumFreq s then newSumFreq else findDuplicate (S.insert newSumFreq s) ls newSumFreq
