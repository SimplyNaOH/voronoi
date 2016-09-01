{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Fortune

import System.Random
import System.IO (hFlush, stdout)

import Text.Read (readMaybe)

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import qualified Diagrams.TwoD.Layout.Tree as T

import Data.List (sortOn)


centers :: Int -> Int -> StdGen -> [(Double, Double)]
centers nCenters nCells gen =
  let
    (ls, rs) = splitAt nCenters $
      take (2*nCenters) $ randomRs (0, fromIntegral nCells) gen
  in
    zip ls rs

testset n gen = sortOn snd $ fmap (\(x,y) -> (x-20,y)) $ centers n 40 gen

{- |
     Given the previous state and current state, draw the Breakpoints Binary
     tree with the following considerations:
       * If the following event is a CircleEvent, render the nodes that
       will be merged in yellow.
       * If the currently processed event is a NewPoint event, render the two
       new nodes in red.
       * If the currently processed event is a CircleEvent, render the new node
       in blue.
       * Otherwise, render the node in white.
-}

renderBTree :: State Double -> State Double -> Diagram B
renderBTree prev current =
  let
    breaks = sbreaks current
    nps = snewpointevents prev
    cevs = scircleevents prev
    nextnps = snewpointevents current
    nextcevs = scircleevents current
    NewPoint pi (px, py) = head nps
    CircleEvent ci cj ck cy _ = head cevs
    NewPoint _ (_, npy) = head nextnps
    CircleEvent nci ncj nck ncy _ = head nextcevs
    currentIsCircle
      | null nps = True
      | null cevs = False
      | otherwise = cy <= py

    nextIsCircle
      | null nextnps && length nextcevs > 1 = True
      | not (length nextcevs > 1) = False
      | length nextnps > 1 = ncy <= npy
      | otherwise = True

    convertX :: (Int, (Int, Int)) -> Diagram B
    convertX x
      | nextIsCircle && (snd x == (nci, ncj) || snd x == (ncj, nck)) =
        circle 0.2 # fc orange # lw none
      | not currentIsCircle && (fst (snd x) == pi || snd (snd x) == pi) =
        circle 0.2 # fc red # lw none
      | currentIsCircle && snd x == (ci, ck) = circle 0.2 # fc blue # lw none
      | otherwise = circle 0.1 # fc white


    convert :: Tree (Int, (Int, Int)) -> T.BTree (Diagram B)
    convert Nil = T.Empty
    convert (Node l x r) = T.BNode (convertX x) (convert l) (convert r)
    btree = convert breaks

--    Just t' = T.uniqueXLayout 0.7 0.8 btree
    Just t' = T.symmLayoutBin' (with & T.slVSep .~ 0.5) btree

    renderT :: Diagram B
--    renderT = translate (r2 (0, 18))$ T.renderTree (\n -> text (init $ tail $ show $ snd n) # fontSizeL 0.3 <> circle 0.3 # fc white)
    renderT = translate (r2 (0, 3))$ T.renderTree (id)
            (~~)
            (t')
          # centerX # pad 1.1

  in
    renderT <> rect 15 15 # lw none

main' state nsteps count =
  let
--    steps = iterate nextEvent state
    new = nextEvent state
    padded x
      | x < 10 = "000" ++ show x
      | x < 100 = "00" ++ show x
      | x < 1000 = "0" ++ show x
      | otherwise = show x
  in do
--    nsteps' <- getLine
    if nsteps > 0 then do
--      let nsteps = read nsteps' :: Int in do
        renderRasterific ("./anim/out_" ++ padded count ++ ".jpg") (dims $ r2
          (400 :: Double, 400)) $ renderBTree state new # bg white
--        putStrLn $ show . fmap snd . inorder . sbreaks $ steps !! nsteps
        main' new (nsteps - 1) (count + 1)
    else
      if nsteps < 0 && not (null (snewpointevents state) && null (scircleevents state)) then
        do
          renderRasterific ("./anim/out_" ++ padded count ++ ".jpg") (dims $ r2
            (400 :: Double, 400)) $ renderBTree state new  # bg white
          main' new nsteps (count + 1)
      else
        putStrLn "Bye!"


main =
  let
    ini n si = (iterate nextEvent $ mkState $ testset n $ mkStdGen 1) !! si
    ini' = liftA2 ini
    run' n si = liftA2 main' (ini' n si)
    usage = unlines ["Parse error.", "All inputs must be integers.",
      "The algorithm will run with 'Number of points' centers, for 'nsteps' \
      \steps, or until the diagram is complete if nsteps = -1, and starting from\
      \ the step number 'inistep'."]
  in do
    putStr "Number of points: "
    hFlush  stdout
    n <- getLine
    putStr "nsteps: "
    hFlush  stdout
    nsteps <- getLine
    putStr "inistep: "
    hFlush  stdout
    si <- getLine
    case run' (readMaybe n) (readMaybe si) (readMaybe nsteps) of
      Nothing  -> putStrLn usage
      Just run -> run 0
