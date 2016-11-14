{-# LANGUAGE StrictData #-}
module Fortune
  ( voronoi
  , Edge' (..)
  )
where

import BreakpointTree


import Control.Monad (liftM, join)

import Data.Maybe (maybeToList, catMaybes)
import Data.List (sortOn)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import qualified Data.HashPSQ as PSQ
import           Data.HashPSQ (HashPSQ)


type Index = Int
type Coord = Double

--data Point = P !Index !Coord !Coord deriving (Show)

type Point'= (Double, Double)

data Edge  = EmptyEdge | IEdge !Point' | Edge !Point' !Point'
data Edge' = Edge' !Index !Index !Point' !Point' deriving (Show)

type NewPointEvent = Point
data CircleEvent   = CircleEvent !Point !Point !Point !Coord !Point'

instance Show CircleEvent where
  show (CircleEvent pi pj pk _ _) = show (pindex pi, pindex pj, pindex pk)


data Events = Events
  {
    newPointEvents :: V.Vector NewPointEvent
  , circleEvents   :: HashPSQ (Index, Index, Index) Coord CircleEvent
  }

data State = State
  {
    events :: Events
  , breaks :: BTree
  , edges :: Map (Index, Index) Edge
  , prevd  :: Double
  }


-- * Private methods
-- ** Manipulating events


{- |
    > removeCEvent i j k events
    Remove a CircleEvent identified by the 3 indexes /i j k/ from /events/.
-}
removeCEvent :: Point -> Point -> Point -> [CircleEvent]
             -> [CircleEvent]
removeCEvent (P i _ _) (P j _ _) (P k _ _) events =
  let
    predicate x = let CircleEvent (P i' _ _) (P j' _ _) (P k' _ _) _ _ = x in
      i' == i && j' == j && k' == k
    (ls, rs) = break predicate events
  in
    if not (null rs) then ls ++ tail rs else ls

{- |
    > insertEvents newEvents events
    Inserts each Event in /newEvents/ into /events/, keeping the list sorted.
 -}
insertEvents :: [CircleEvent] -> [CircleEvent] -> [CircleEvent]
insertEvents toAdd events =
  let
    insertEvent toAdd' events' = 
      let
        CircleEvent _ _ _ y _ = toAdd'
        (ls, rs) = span (\(CircleEvent _ _ _ y' _) -> y' < y) events'
      in
        if y /= 0 then
          ls ++ toAdd' : rs
        else
          events'
  in
    foldr insertEvent events toAdd

-- ** Helper Functions

pindex (P i _ _) = i
breakNull (Breakpoint (P i _ _) (P j _ _)) = i == 0 && j == 0
pointAtLeftOf (Breakpoint l _) = l
pointAtRightOf (Breakpoint _ r) = r

sortPair a b = if a < b then (a, b) else (b, a)

setVert :: Point' -> Edge -> Edge
setVert p EmptyEdge  = IEdge p
setVert p (IEdge p') = Edge p' p

-- | Returns (Just) the (center, radius) of the circle defined by three given
--   points.
--   If the points are colinear or counter clockwise, it returns Nothing.
circleFrom3Points :: Point -> Point -> Point -> Maybe (Point', Double)
circleFrom3Points (P _ x1 y1) (P _ x2 y2) (P _ x3 y3) =
  let
    (bax, bay) = (x2 - x1, y2 - y1)
    (cax, cay) = (x3 - x1, y3 - y1)
    ba = bax * bax + bay * bay
    ca = cax * cax + cay * cay
    denominator = 2 * (bax * cay - bay * cax)

    x = x1 + (cay * ba - bay * ca) / denominator
    y = y1 + (bax * ca - cax * ba) / denominator
    r = sqrt $ (x-x1) * (x-x1) + (y-y1) * (y-y1)
  in
    if denominator <= 0 then
      Nothing -- colinear points or counter clockwise
    else
      Just ((x, y), r)

circleEvent :: Point -> Point -> Point -> Maybe CircleEvent
circleEvent pi pj pk = liftM (\(c@(_, y), r) -> CircleEvent pi pj pk (y + r) c)
  $ circleFrom3Points pi pj pk


-- ** Processing events

processCircleEvent :: State -> State
processCircleEvent state = let
  -- state data:
  Just (_, _, (CircleEvent pi@(P i _ _) pj@(P j _ _) pk@(P k _ _) y p), cevents) =
    PSQ.minView . circleEvents . events $ state
  events' = events $ state
  bTree = breaks state
  d     = y
  d'    = (d + prevd  state) / 2

  -- process breakpoint
  bl = Breakpoint pi pj
  br = Breakpoint pj pk
  newBreak = Breakpoint pi pk
  newBTree = joinPairAt (fst p) bl br d d' bTree

  -- process events
  prevB@(Breakpoint prev@(P previ _ _) (P prevj _ _)) = inOrderPredecessor bl d' bTree
  nextB@(Breakpoint (P nexti _ _) next@(P nextj _ _)) = inOrderSuccessor   br d' bTree

  newCEvents'
    | previ == 0 && prevj == 0 =
      maybeToList $ circleEvent pi pk next
    | nexti == 0 && nextj == 0 =
      maybeToList $ circleEvent prev pi pk
    | otherwise =
      catMaybes [circleEvent pi pk next, circleEvent prev pi pk]
  
  toRemove
    | previ == 0 && prevj == 0 =
      [(i, j, k), (j, k, nextj)]
    | nexti == 0 && nextj == 0 =
      [(i, j, k), (previ, i, j)]
    | otherwise =
      [(i, j, k), (previ, i, j), (j, k, nextj)]

  insert' ev@(CircleEvent pi pj pk y _) =
    PSQ.insert (pindex pi, pindex pj, pindex pk) y ev
  removed = foldr PSQ.delete cevents toRemove
  newCEvents = foldr insert' removed newCEvents'

  newEvents = events' { circleEvents = newCEvents }

  -- process edge
  newEdge = IEdge p
  edgesToUpdate = [sortPair (pindex pi) (pindex pj),
                   sortPair (pindex pj) (pindex pk)]
  updatedEdges = foldr (Map.adjust (setVert p)) (edges state) edgesToUpdate
  newEdges = Map.insert (sortPair (pindex pi) (pindex pk)) newEdge updatedEdges

  pretty (a, b, c) = (pindex a, pindex b, pindex c)
  in 
    state { breaks = newBTree, events = newEvents, edges = newEdges, prevd = d }

processNewPointEvent :: State -> State
processNewPointEvent state = let
  -- state data:
  newp@(P idx _ d) = V.head . newPointEvents . events $ state
  newPEvents       = V.tail . newPointEvents . events $ state
  cEvents = circleEvents . events $ state
  events' = events state
  bTree = breaks state

  (newBTree, fallenOn) = insertPar newp d bTree
  (prev, next) = case fallenOn of
    Left  b -> (inOrderPredecessor b d bTree, b)
    Right b -> (b, inOrderSuccessor b d bTree)
    -- TODO !! inOrderPred.. and inOrderSucc.. shouldn't need d

  pi = if breakNull prev then Nothing else Just $ pointAtLeftOf prev
  pk = if breakNull next then Nothing else Just $ pointAtRightOf next
  pj = case fallenOn of
    Left b -> pointAtLeftOf b
    Right b -> pointAtRightOf b
  
  newCEvents' = catMaybes [ do pi <- pi; circleEvent pi pj newp
                          , do pk <- pk; circleEvent newp pj pk]
  
  toRemove = (pi, pj, pk)

  insert' ev@(CircleEvent pi pj pk y _) =
    PSQ.insert (pindex pi, pindex pj, pindex pk) y ev
  removed = case toRemove of
    (Just i, j, Just k) -> PSQ.delete (pindex i, pindex j, pindex k) cEvents
    _ -> cEvents
  newCEvents = foldr insert' removed newCEvents'

  newEvents = events' { newPointEvents = newPEvents
                      , circleEvents = newCEvents }

  newEdges = Map.insert (sortPair idx (pindex pj)) EmptyEdge $ 
    edges state

  in
    state { breaks = newBTree, events = newEvents, edges = newEdges, prevd = d }

processEvent :: State -> State
processEvent state
  | (V.null . newPointEvents . events) state && 
    (PSQ.null . circleEvents . events) state = state
  | otherwise = 
    if nextIsCircle then
      processCircleEvent state
    else
      processNewPointEvent state
  where
    (P _ _ nextPointY) = V.head .  newPointEvents . events $ state
    (Just (_, nextCircleY, _)) = PSQ.findMin . circleEvents . events $ state
    nextIsCircle
      | (V.null . newPointEvents .events) state = True
      | (PSQ.null . circleEvents . events) state = False
      | otherwise = nextCircleY <= nextPointY

{- |
    voronoi takes a Vector of pairs of Double(s) and returns a Vector of
    Edge(s) representing the corresponding voronoi diagram.
-}
voronoi :: [Point'] -> [Edge']
voronoi points =
  let
    go :: State -> [Edge']
    go state = if ((null.newPointEvents.events) state) &&
      ((null.circleEvents.events) state) then
      mapToList . edges $ state
    else
      go (processEvent state)
  in
    go . mkState $ points

mkState :: [Point'] -> State
mkState points = let
  ps = sortOn snd points
  newPEvents' = (V.imap (\i (x, y) -> P i x y)) . V.fromList $ ps
  newPEvents = V.tail . V.tail $ newPEvents'
  p0@(P i _ _) = (newPEvents' V.! 0)
  p1@(P j _ d) = (newPEvents' V.! 1)
  b1 = Breakpoint p0 p1
  b2 = Breakpoint p1 p0
  firstPair = Node Nil b1 $ Node Nil b2 Nil
  firstEdge = Map.singleton (sortPair i j) EmptyEdge
  in
    State (Events newPEvents PSQ.empty) firstPair firstEdge d

mapToList map = let
  list' = Map.toList map
  predicate (_, e) = case e of
    Edge _ _ -> True
    _ -> False
  list = filter predicate list'
  edge' ((i, j), Edge l r) = Edge' i j l r
  in
    fmap edge' list
