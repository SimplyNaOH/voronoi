{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Fortune


import Debug.Trace (trace)

import Control.Arrow ((&&&), (***))

import System.Random

import Data.Word
import Data.List (sortOn, sortBy, nub, zipWith4)
import Data.Maybe (fromJust)


centers :: Int -> Int -> StdGen -> [(Double, Double)]
centers nCenters nCells gen =
  let
    (ls, rs) = splitAt nCenters $
      take (2*nCenters) $ randomRs (0, fromIntegral nCells) gen
  in
    zip ls rs

testset n gen = sortOn snd $ fmap (\(x,y) -> (x-20,y)) $ centers n 40 gen


main =
  let 
    gen = mkStdGen 1
    n = 48
    points = testset n
    final gen = voronoi $ points gen
  in --do
    --gen <- newStdGen
    putStrLn $ show $ final gen
