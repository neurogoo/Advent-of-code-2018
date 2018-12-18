module Day10 where

import           Control.Monad        (foldM, void)
import           Data.Either
import           Data.Foldable
import           Data.Traversable

import qualified Data.Attoparsec.Text as DT
import qualified Data.Set             as Set
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

data Light = Light
    { getX    :: !Int
    , getY    :: !Int
    , getVelX :: !Int
    , getVelY :: !Int
    } deriving Show

day10 :: IO ()
day10 = do
    input <- TIO.readFile "/Users/toku/Haskell/Advent-of-code-2018/inputs/day10.txt"
    let movedLights = (iterate (fmap addVel) (rights $ DT.parseOnly parseLight <$> T.lines input))
        minimumXSec = minimumBy (\a b -> compare (snd a) (snd b)) $ zip [0 ..] (xrange <$> take 15000 movedLights)
        minimumYSec = minimumBy (\a b -> compare (snd a) (snd b)) $ zip [0 ..] (yrange <$> take 15000 movedLights)
    print (fst minimumXSec)
    prettyPrintMessage $ (iterate (fmap addVel) (rights $ DT.parseOnly parseLight <$> T.lines input)) !! (fst minimumXSec)
  where
      addVel (Light x y velx vely) = Light (x + velx) (y + vely) velx vely
      xrange lights = (maximum $ getX <$> lights) - (minimum $ getX <$> lights)
      yrange lights = (maximum $ getY <$> lights) - (minimum $ getY <$> lights)

parseLight :: DT.Parser Light
parseLight = do
    void $ DT.string "position=<"
    void $ DT.skipWhile (\x -> x == ' ' || x == ',')
    getX <- DT.signed DT.decimal
    void $ DT.skipWhile (\x -> x == ' ' || x == ',')
    getY <- DT.signed DT.decimal
    void $ DT.string "> velocity=<"
    void $ DT.skipSpace
    getVelX <- DT.signed DT.decimal
    void $ DT.skipWhile (\x -> x == ' ' || x == ',')
    getVelY <- DT.signed DT.decimal
    pure $ Light getX getY getVelX getVelY

prettyPrintMessage :: [Light] -> IO ()
prettyPrintMessage lights = let minX = minimum $ getX <$> lights
                                maxX = maximum $ getX <$> lights
                                minY = minimum $ getY <$> lights
                                maxY = maximum $ getY <$> lights
                                pointSet = Set.fromList $ (\(Light x y _ _) -> (x,y)) <$> lights
                            in forM_ [minY .. maxY] $ \y -> do
                                 putStrLn $ (concat $ for [minX .. maxX] $ \x ->
                                     if Set.member (x,y) pointSet then pure '#' else pure '.' :: String)
