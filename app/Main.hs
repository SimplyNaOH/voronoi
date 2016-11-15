{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Fortune


import Debug.Trace (trace)

import Control.Arrow ((&&&), (***))

import System.Random

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

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
colors n gen =
  let
    (rs, rand)  = splitAt n $ randomRs (0, 255 :: Word8) gen
    (gs, rand') = splitAt n rand
    bs          = take n rand'
  in
    zipWith3 sRGB24 rs gs bs

range = [-40,-39.9..40]


toDouble :: (Real a) => (a, a) -> (Double, Double)
toDouble (x, y) = (realToFrac x, realToFrac y)

line :: (Real a) => [(a, a)] -> Diagram B
line points = strokeLocLine $ at (fromVertices $ fmap (p2 . toDouble) points) ((p2 . toDouble . head) points)

polygon' :: (Real a) => [(a, a)] -> Diagram B
polygon' points = strokeLocLoop $ at (closeLine $ fromVertices $ fmap (p2 . toDouble) points) ((p2 . toDouble . head) points)


renderedges :: [Edge'] -> [Diagram B]
renderedges edges = 
  concatMap (\(Edge' _ _ l r) -> if l /= (0,0) && r/=(0,0) then
      [line [l, r]]
    else
      []) edges
 

polygonFrom i points edges =
  let
    edges' = filter (\(Edge' a b _ _) -> a == i || b == i) edges
    vertices = nub $ concatMap (\(Edge' _ _ l r) -> [l,r]) edges'
    xs = fmap ((id &&& id) . fst) vertices
    ys = fmap ((id &&& id) . snd) vertices
    n  = fromIntegral $ length vertices
    (centerx, centery) = (/n) *** (/n) $ foldl1 (\(x, y) (a, b) -> (x+a, y+b)) vertices

    (minY, maxY) = foldl1 (\(a,x) (b,y) -> (min a b, max x y)) ys
    midY = (maxY - minY) / 2 + minY

    orderCW (ax, ay) (bx, by)
      | ax - centerx >= 0 && bx - centerx <  0 = LT
      | ax - centerx <  0 && bx - centerx >= 0 = GT
      | ax - centerx == 0 && bx - centerx == 0 = compare ay by
      | det < 0 = LT
      | det > 0 = GT
      | otherwise = compare d1 d2
      where
        det = (ax - centerx) * (by - centery) - (bx - centerx) * (ay - centery)
        d1  = (ax - centerx) * (ax - centerx) + (ay - centery) * (ay - centery)
        d2  = (bx - centerx) * (bx - centerx) + (by - centery) * (by - centery)

    sorted = sortBy orderCW vertices

  in
    polygon' sorted

--render :: (Real a) => [(a, a)] -> [Edge' a] -> [Color] -> Diagram
render' points edges colors = 
  let
    centers :: [Diagram B]
    centers = fmap (\p -> circle 0.1 # fc white # lw 1 # translate (r2 . toDouble $ p))
      points
    polygons = fmap (\i -> fc (colors !! i) $  polygonFrom i points edges) [0..(length points - 1)]
  in
    foldl1 atop $ centers ++ polygons -- ++ renderedges edges


main =
  let 
    --gen = mkStdGen 1
    --n = 48
    points n = testset n
    colors' n = colors n
    final n gen = voronoi $ points n gen
    line :: Diagram B
    line = strokeLine $ fromVertices $ [p2 (0,0), p2 (1,5)]
  in do
    gen <- newStdGen
    n <- getLine
--    putStrLn $ show $ points (read n) gen
--    putStrLn $ seq (final (read n) gen) "done"
--    putStrLn $ show $ final (read n) gen
    mainWith ( (render' (points (read n) gen) (final (read n) gen) (colors' (read n) gen)) # rectEnvelope (p2 (-22,-2)) (r2 (44, 44))  :: Diagram B)
--    mainWith ( (foldl1 atop (renderedges final)) # rectEnvelope (p2 (-30,-10)) (r2 (60, 60))  :: Diagram B)
