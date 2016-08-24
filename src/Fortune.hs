{-# OPTIONS_HADDOCK ignore-exports #-}
module Fortune
  ( voronoi
  , Point (..)
  , Edge (..)
  )
where

import Debug.Trace (trace)

import Control.Arrow ((***), (&&&))

import Data.List

import Data.Maybe (isNothing, fromJust, maybeToList, catMaybes)

import Data.Tuple (swap)


type Index = Int

type Point a = (a, a)

data Event a = NewPoint Index (Point a)
           | CircleEvent Index Index Index a (Point a)
           deriving Show

data Type = L | R deriving Show

data Breakpoint a = Breakpoint Index Index (Point a) Type deriving Show

data Edge a = Edge Index Index (Point a) (Point a) deriving Show

data State a = State
  {
    spoints :: [Point a]
  , sevents :: [Event a]
  , sbreaks :: [Breakpoint a]
  , sedges  :: [Edge a]
  , sfirst  :: Index
  } deriving Show



{- |
    Generate the voronoi diagram (defined by a set of edges) corresponding to
    the given list of centers.
-}
voronoi :: (Floating a, Ord a) => [Point a] -> [Edge a]
voronoi points =
  let
    go :: (Floating a, Ord a) => State a -> [Edge a]
    go state = if null (sevents state) then
        sedges $ finish state
        --state
      else
        go (nextEvent state)
  in
    go $ mkState points



-- * Private methods
-- ** Manipulating events


{- |
    > removeCEvent i j k events
    Remove a CircleEvent identified by the 3 indexes /i j k/ from /events/.
-}
removeCEvent :: (Floating a) => Index -> Index -> Index -> [Event a] 
             -> [Event a]
removeCEvent i j k events =
  let
    removeFromList x xs = let (ls,rs) = splitAt x xs in ls ++ tail rs
    index = findIndex (search) events
    search event = case event of
      CircleEvent i' j' k' _ _ -> [i',j',k'] == [i,j,k]
      _ -> False
  in
   case index of
     Nothing -> events
     Just idx -> removeFromList idx events

{- |
    > insertEvents newEvents events
    Inserts each Event in /newEvents/ into /events/, keeping the list sorted.
 -}
insertEvents :: (Floating a, Ord a) => [Event a] -> [Event a] -> [Event a]
insertEvents news events =
  let
    insertEvent new events' = 
      let
        CircleEvent _ _ _ y _ = new
        (ls, rs) = span (\x -> case x of
          CircleEvent _ _ _ y' _ -> y' < y
          NewPoint _ (_,y') -> y' < y) events'
      in
        if y /= 0 then
          ls ++ new : rs
        else
          events'
  in
    foldr insertEvent events news


-- ** Breakpoints

indexAtLeftOf :: Breakpoint a -> Index
indexAtLeftOf  (Breakpoint l _ _ _) = l

indexAtRightOf :: Breakpoint a -> Index
indexAtRightOf (Breakpoint _ r _ _) = r

{- |
    > updateBreakpoints d state
    Updates the breakpoints in the /state/ at a sweeping coordinate /d/.
    It needs a State to retrieve the coordinates of the centers and evaluate the
    corresponding parabolas.
-}
updateBreakpoints :: (Floating a, Ord a) => a -> State a -> [Breakpoint a]
updateBreakpoints d state =
  let
    breaks = sbreaks state
    -- Breakpoints are always taken from left to right (increasing x). So which
    -- of the two intersections between parabolas i and j we take, depends on
    -- the y-coordinate of the focus point of each parabola:
    update (Breakpoint i j _ t)
      | snd pi < snd pj = Breakpoint i j leftmost t
      -- if the leftmost parabola has a focus point further away from the
      -- sweeping line (which is necessarily greater than all points being
      -- considered) we take the leftmost intersection.
      | otherwise = Breakpoint i j rightmost t
      -- otherwise we take the rightmost
      where
        pi = spoints state !! i
        pj = spoints state !! j
        (leftmost, rightmost) = intersection pi pj d
  in
    fmap update breaks


{- |
    > joinBreakpoints p i breaks
    Join breakpoint /i/ and /i+1/ at the point /p/. Joining two breakpoints
    results in a new breakpoint, with a corresponding new edge, and possible new
    events, as well as potentially events that need to be removed.
-}
joinBreakpoints :: (Floating a, Ord a) => Point a -> Index -> [Breakpoint a] -> [Point a]
                -> ([Breakpoint a], Edge a, [Event a], [(Index, Index, Index)])
joinBreakpoints p i breaks points =
  let
    (Breakpoint l c _ _) = breaks !! i -- l = left, c = center
    (Breakpoint _ r _ _) = breaks !! (i+1) -- r = right
    (ls, rs) = splitAt i breaks
    newbreak = Breakpoint l r p R
    newbreaks = ls ++ newbreak:(drop 2 rs)
    newedge = edge l r p p
    -- The following two may fall out of bounds, but are only used when we are
    -- sure they exist:
    prev = indexAtLeftOf  $ breaks !! (i-1)
    next = indexAtRightOf $ breaks !! (i+2)

    (newevents, toremove)
      | i == 0 = 
        ( maybeToList $ circleEvent l r next points
        , [(l, c, r), (c, r, next)] )

      | i == length breaks - 2 =
        ( maybeToList $ circleEvent prev l r points
        , [(l, c, r), (prev, l, c)] )
      | otherwise = 
        ( catMaybes [circleEvent l r next points, circleEvent prev l r points]
        , [(l, c, r), (prev, l, c), (c, r, next)] )
  in
    (newbreaks, newedge, newevents, toremove)

-- ** Processing events

{-|
   Process a NewPoint Event. It will result in a new set of breakpoints, a new
   edge, and potentially new events and events to be removed.
-}
processNewPoint :: (Floating a, Ord a) => State a-> State a
processNewPoint state =
  let
    (NewPoint idx p) = head . sevents $ state
    breaks = updateBreakpoints (snd p) state
    points = spoints state
    (ls, rs) = span (\(Breakpoint _ _ (x,_) _) -> x < fst p) breaks
    
    -- There is a special case for the first set of breakpoints:
    firstPair = [ Breakpoint (sfirst state) idx (fst p, 0) L
                , Breakpoint idx (sfirst state) (fst p, 0) R]
    firstEdge = edge (sfirst state) idx (fst p, 0) (fst p, 0)

    -- If this is not the first pair of breakpoints:

    -- In the following lines, centerIndex is the index of the center whose
    -- parabolic section the new breakpoints land on. leftIndex and rightIndex
    -- represent the indexes of the centers of the previous and following
    -- parabolic sections to the center one, if there are any, or Nothing.

    leftIndex   = if null ls then Nothing else Just $ indexAtLeftOf  $ last ls
    rightIndex  = if null rs then Nothing else Just $ indexAtRightOf $ head rs
    centerIndex = if null ls then indexAtLeftOf  $ head rs
                             else indexAtRightOf $ last ls

    newPair = [ Breakpoint centerIndex idx (fst p, 0) L
              , Breakpoint idx centerIndex (fst p, 0) R]

    newEdge = edge idx centerIndex (0, 0) (0, 0)

    
    -- Helper function to create a circle event where the first or last index
    -- might be Nothing.
--    circleEvent' :: Maybe Index -> Index -> Maybe Index -> [Event a]
    circleEvent' i' j k' = case (i', k') of
      (Just i, Just k) -> maybeToList $ circleEvent i j k points
      _ -> []

    -- newEvents' might be a list of length 1 or 2, but should never be an empty
    -- list, as the first pair of breakpoints is  treated separately.
    newEvents' = circleEvent' leftIndex  centerIndex (Just idx) ++
                 circleEvent' (Just idx) centerIndex rightIndex

    -- toRemove :: (Maybe Index, Index, Maybe Index)
    toRemove = (leftIndex, centerIndex, rightIndex)
    
    -- we join the pieces together to form the new list:
    breakswithNewPair = ls ++ newPair ++ rs

    -- Here are all the final values, which take into account wether we are in
    -- the first pair of breakpoints or not:
    newEdges
      | null breaks = [firstEdge]
      | otherwise   = newEdge : sedges state

    newEvents
      | null breaks = tail $ sevents state
      | otherwise   = insertEvents newEvents' $
        (case toRemove of
          (Just i, j, Just k) -> removeCEvent i j k
          _ -> id)  $ tail $ sevents state

    newBreaks
      | null breaks = firstPair
      | otherwise   = breakswithNewPair


    circleEvents evs = length $ filter (\x -> case x of CircleEvent {} -> True; _ -> False) evs
    removed = circleEvents (tail $ sevents state) - circleEvents newEvents + length newEvents'
  in
    trace (concat $ intersperse " " $ fmap show $ [circleEvents newEvents, length newEvents', removed]) $
      state { sbreaks = newBreaks, sedges = newEdges, sevents = newEvents }

{- |
    Process a CircleEvent Event. It will join the converging breakpoints and
    adjusts the events and edges accordingly.
-}
processCircleEvent :: (Floating a, Ord a) => State a -> State a
processCircleEvent state = 
  let
    (CircleEvent i j k y p) = head $ sevents state
    breaks = updateBreakpoints y state
    points = spoints state

    -- We pair up the breakpoints, and calculate the three parabolic section
    -- they associate:
    pairs = fmap (\a -> (breaks !! a, breaks !! (a + 1)))
      [0..(length breaks - 2)]
    associatedSections = fmap (\(l, r) ->
      [indexAtLeftOf l, indexAtRightOf l, indexAtRightOf r]) pairs
    -- The following line assumes the algorithm has done nothing wrong, and thus
    -- we know for sure that if we are processing a CircleEvent, it's because
    -- the associated pair of breakpoints exists! If the "fromJust" fails, it
    -- means that there is an error in the implementation of the algorithm.
    pairIndex = fromJust $ elemIndex [i, j, k] associatedSections
    
    bs = [breaks !! pairIndex, breaks !! (pairIndex+1)]

    -- helper function to edit Lists:
    modifyList pos ele list = let (ls,rs) = splitAt pos list in
      ls ++ ele:tail rs

    (newBreaks, newEdge, newEvents', toRemove) =
      joinBreakpoints p pairIndex breaks points

    uncurry3 f (a,b,c) = f a b c
    newEvents = insertEvents newEvents' $
      foldr (uncurry3 removeCEvent) (tail $ sevents state) toRemove
    
    setVert (Breakpoint l r _ t) edges = 
      case t of
        L -> modifyList index (left edge) edges
        R -> modifyList index (right edge) edges
        where
          index = fromJust $ findIndex (\(Edge a b _ _) -> a == min l r && b == max l r) edges
          edge = edges !! index
          left  (Edge i j _ r) = Edge i j p r
          right (Edge i j l _) = Edge i j l p

    newEdges = newEdge : foldr setVert (sedges state) bs

    circleEvents evs = length $ filter (\x -> case x of CircleEvent {} -> True; _ -> False) evs
    removed = circleEvents (tail $ sevents state) - circleEvents newEvents + length newEvents'
  in
    trace (concat $ intersperse " " $ fmap show $ [circleEvents newEvents, length newEvents', removed]) $
--    trace ("\n" ++ show (length pairindices) ++ " :.: " ++ show (length actualpairs))
--    trace ("\n" ++ show ids ++ "\n" ++ show bs ++ "\n" ++ show pairindices ++ "\n") $
      state { sbreaks = newBreaks, sevents = newEvents, sedges = newEdges } 

-- ** Algorithm

{- |
    Advance the sweeping line to the next Event. Just applies the corresponding
    processing function to the next event.
-}
nextEvent :: (Floating a, Ord a) => State a -> State a
nextEvent state
  | null (sevents state) = state
  | otherwise =
    case head (sevents state) of
      NewPoint {} -> processNewPoint state
      CircleEvent {} -> processCircleEvent state

{- |
    After finishing processing all events, we may end up with breakpoints that
    extend to infinity. This function trims those edges to a bounding box 10
    units bigger than the most extreme vertices.
-}
finish :: (Floating a, Ord a) => State a -> State a
finish state
  | null (sbreaks state) = state
  | otherwise =
    let
      breaks = updateBreakpoints (maxY + 20) state
      edges = sedges state
      points = spoints state

      -- min* and max* hold the extreme values for the edges, while min*' and
      -- max*' hold those of the points. This code will figure out which way to
      -- extend the edge based on the maximum and minimum values of the points.
      -- That is to say, if for example our x value is nearest to the maximum x
      -- value of the points, then we will extend to the right (up until maxX,
      -- the maximum x value of the known edges). In the end, all vertices will
      -- be bounded to (minX, minY) (maxX, maxY) which is the original bounding
      -- box plus 20 units on each side.

      xs = (\x -> (x, x)) <$>
        concatMap (\(Edge _ _ (x, _) (x', _)) -> [x, x']) edges
      ys = (\x -> (x, x)) <$>
        concatMap (\(Edge _ _ (_, y) (_, y')) -> [y, y']) edges
      (minX, maxX) = (\(a, b) -> (a - 20, b + 20)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) xs
      (minY, maxY) = (\(a, b) -> (a - 20, b + 20)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) ys

      xs' = (\x -> (x, x)) <$>
        concatMap (uncurry $ flip (:) . (:[])) points
      ys' = (\x -> (x, x)) <$>
        concatMap (uncurry $ flip (:) . (:[])) points
      (minX', maxX') = (\(a, b) -> (a, b)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) xs'
      (minY', maxY') = (\(a, b) -> (a, b)) $
        foldl1 (\(a,x) (b,y) -> (min a b, max x y)) ys'

      
      inRangeY b = b > minY && b < maxY
      nearest a (b, c) (d, e) = if abs (a - b) < abs (a - c)
        then d else e

      -- The guard here is to prevent trying to use the equation for a straight
      -- line in the case of a (almost) horizontal or (almost) vertical line, as
      -- the slope would be infinite. "xc" and "yc" are the "corrected" x and y
      -- value (bounded to the bounding box). We use xc if the corresponding
      -- y-value falls into rante, or yc with its corresponding x-value.

      restrict (x1,y1) (x',y')
        | abs (x1 - x') > 0.00001 && abs (y1 - y') > 0.00001 =
          if inRangeY (snd restrictX) then restrictX else restrictY
        | abs (x1 - x') <= 0.00001 =
          (x', yc)
        | otherwise =
          (xc, y')
        where
          restrictX = (xc, (xc - x1)*(y1 - y')/(x1 - x') + y1)
          restrictY = ((yc - y1)*(x1 - x')/(y1 - y') + x1, yc)
          xc = nearest x1 (maxX', minX') (maxX, minX) 
          yc = nearest y1 (maxY', minY') (maxY, minY) 

      modifyList pos ele list = let (ls,rs) = splitAt pos list in
        ls ++ ele:tail rs
      
      setVert (Breakpoint l r p t) edges = case t of
        L -> modifyList index (left edge) edges
        R -> modifyList index (right edge) edges
        where
          index = head $ findIndices (\(Edge a b _ _) -> a == min l r && b == max l r) edges
          edge = edges !! index
          left  (Edge i j _ r) = Edge i j (restrict r p) r
          right (Edge i j l _) = Edge i j l (restrict l p)
    in
      state { sedges = foldr setVert (sedges state) breaks }

{- |
    Create an initial state from a given set of centers.
-}
mkState :: (Floating a, Ord a) => [Point a] -> State a
mkState points =
  let
    sorted = sortOn (snd.snd) $
      foldl (\acc x -> (length acc, x):acc)  [] points
    events = tail $ fmap (uncurry NewPoint) sorted
  in
    State points events [] [] (fst $ head sorted)


-- ** Helper functions

-- | Smart constructor of Edge: it ensures that the indexes are sorted.
edge :: (Floating a) => Index -> Index -> Point a -> Point a -> Edge a
edge i j = Edge (min i j) (max i j) 

-- | Given three indexes and the list of points, check if the three points at
-- the indexes form a circle, and create the corresponding CircleEvent.
circleEvent :: (Floating a, Ord a) => Index -> Index -> Index -> [Point a] -> Maybe (Event a)
circleEvent i j k points = case circle of
    Just (c@(_, y), r) -> Just $ CircleEvent i j k (y + r) c
    _ -> Nothing
  where
    circle = circleFrom3Points (points !! i) (points !! j) (points !! k)
-- | 'evalParabola focus directrix x' evaluates the parabola defined by the
-- focus and directrix at x
evalParabola :: (Floating a) => Point a -> a -> a -> Point a
evalParabola (fx, fy) d x = (x, (fx*fx-2*fx*x+fy*fy-d*d+x*x)/(2*fy-2*d))

{- |
    > intersection f1 f2 d
    Find the intersection between the parabolas with focus /f1/ and /f2/ and
    directrix /d/. The resulting points are ordered in increasing x-coordinate.
-}
intersection :: (Floating a, Ord a) => Point a -> Point a -> a -> (Point a, Point a)
intersection (f1x, f1y) (f2x, f2y) d =
  let
    dist = (f1x - f2x) * (f1x - f2x) + (f1y - f2y) * (f1y-f2y)
    sqroot = sqrt $ dist * (f1y - d) * (f2y - d)
    lastterm = f1x * (d - f2y) - f2x * d
    x1' = (f1y*f2x - sqroot + lastterm)/(f1y - f2y)
    x2' = (f1y*f2x + sqroot + lastterm)/(f1y - f2y)
    x1 = min x1' x2'
    x2 = max x1' x2'
  in
    (evalParabola (f1x, f1y) d x1, evalParabola (f1x, f1y) d x2)

-- | Returns (Just) the (center, radius) of the circle defined by three given points.
-- If the points are colinear or counter clockwise, it returns Nothing.
circleFrom3Points :: (Floating a, Ord a) => Point a -> Point a -> Point a -> Maybe (Point a, a)
circleFrom3Points (x1, y1) (x2, y2) (x3,y3) =
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

