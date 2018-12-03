{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import           Control.Monad.ST
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Char
import           Data.List
import           Data.STRef
import           Data.Traversable

import qualified Data.Map          as M

data Claim = Claim
    { claimId :: String
    , startX  :: Int
    , startY  :: Int
    , width   :: Int
    , height  :: Int
    } deriving (Show)

day3_1 :: IO Int
day3_1 = do
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day3.txt"
    let claims = (toClaim . words) <$> lines input
        size = 1000
        cloth = newArray (1, size * size) 0 :: ST s (STArray s Int Int)
        helper arr (x,y) = do
            a <- readArray arr (x + y*size)
            writeArray arr (x + y*size) (a+1)
        result = runST $ do
            arr <- cloth
            _ <- for claims $ \(Claim cid startX startY width height) ->
                mapM (helper arr) [(x,y) | x <- [startX..(startX + width - 1)], y <- [startY .. (startY + height - 1)]]
            elems <- getElems arr
            pure elems
    pure $ length $ filter (>= 2) result
    where
      toClaim [claimId,separator,startPos,size] =
          Claim
          claimId
          ((read :: String -> Int) $ fst $ break (\x -> x == ',') startPos)
          ((read :: String -> Int) $ filter isDigit $ snd $ break (\x -> x == ',') startPos)
          ((read :: String -> Int) $ fst $ break (\x -> x == 'x') size)
          ((read :: String -> Int) $ filter isDigit $ snd $ break (\x -> x == 'x') size)

day3_2 :: IO String
day3_2 = do
    input <- readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day3.txt"
    let claims = (toClaim . words) <$> lines input
        size = 1000
        cloth = newArray (1, size * size) "" :: ST s (STArray s Int String)
        helper arr idMap cid (x,y) = do
            a <- readArray arr (x + y*size)
            case a of
              "" -> writeArray arr (x + y*size) cid
              oldCid -> do
                  modifySTRef' idMap (\m -> M.insert oldCid False m)
                  modifySTRef' idMap (\m -> M.insert cid False m)
                  writeArray arr (x + y*size) cid
        result = runST $ do
            arr <- cloth
            keyMap <- newSTRef (M.fromList $ (\c -> (claimId c, True)) <$> claims )
            _ <- for claims $ \(Claim cid startX startY width height) ->
                mapM (helper arr keyMap cid) [(x,y) | x <- [startX..(startX + width - 1)], y <- [startY .. (startY + height - 1)]]
            updatedKeyMap <- readSTRef keyMap
            pure updatedKeyMap
    pure $ fst $ head $ filter (\(a,b) -> b == True) $ M.toList result
    where
      toClaim [claimId,separator,startPos,size] =
          Claim
          claimId
          ((read :: String -> Int) $ fst $ break (\x -> x == ',') startPos)
          ((read :: String -> Int) $ filter isDigit $ snd $ break (\x -> x == ',') startPos)
          ((read :: String -> Int) $ fst $ break (\x -> x == 'x') size)
          ((read :: String -> Int) $ filter isDigit $ snd $ break (\x -> x == 'x') size)
