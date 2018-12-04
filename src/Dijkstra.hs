module Dijkstra where

import Control.Monad.State.Lazy
import Control.Applicative
import Data.List
-- Input: directed graph G = (V, E) in adjacency-list
-- representation, a vertex s in V , a length le >= 0 for each e in E.

-- Postcondition: for every vertex v, the value len(v)
-- equals the true shortest-path distance dist(s, v).

{- Pseudocode:
// Initialization
X := {s}
len(s) := 0, len(v) := +infinite for every v /= s

// Main loop
while there is an edge (v, w) with v in X, w notin X do:
  (v*,w*) := such an edge minimizing len(v) + l_vw
  add w* to X
  len(w*) := len(v*) + l_v*w*
-}

type Distance = Int
type VertixIds = [(Id, Distance)]
type Id = Int
type Graph = [Vertex]
type Edge = (Int, Int)
type EdgeWithLengh = (Int, Int, Int)

data Vertex = Vertex Id [(Int, Int)] deriving Show

testGraph = [Vertex 1 [(2, 1), (3, 4)],
             Vertex 2 [(3, 2), (4, 6)],
             Vertex 3 [(4, 3)],
             Vertex 4 []]
-- should return ([(1, 0), (2, 1), (3,3), (4, 6)]

{-
init, [V_1] [(1, 0)] ->
v* = V_1, w* = V_2, [V_1, V_2] [(1, 0), (2, 0 + 1)] ->
v* = V_2, w* = V_3, [V1, V2, V3] [(1, 0), (2, 1), (3, 1 + 2)] ->
v* = V_3, w* = V_4, [V1, V2, V3, V4] [(1,0), (2,1), (3,3), (4, )]
-}

testGraph2 = [Vertex 1 [(2, 1), (3, 4)],
              Vertex 2 [(3, 2), (4, 3)],
              Vertex 3 [(4, 6)],
              Vertex 4 []]
-- should return ([(1, 0), (2, 1), (3,3), (4, 4)]

{-
init, [V_1] [(1, 0)] ->
v* = V_1, w* = V_2, [V_1, V_2] [(1, 0), (2, 0 + 1)] ->
v* = V_2, w* = V_3, [V1, V2, V3] [(1, 0), (2, 1), (3, 1 + 2)] ->
v* = V_2, w* = V_4, [V1, V2, V3, V4] [(1,0), (2,1), (3,3), (4, 1 + 3)]
-}

type X = [(Int, Int)]
type DjState = State [EdgeWithLengh] (Maybe X)
type DjState' = State (Maybe [EdgeWithLengh]) X

dj :: Graph -> Id -> Maybe X
dj graph gid = evalState (doDj (pure $ pure [(gid, 0)])) (edgesWithDistances graph)

doDj :: DjState -> DjState
doDj theState = do
  edgesWithL <- get
  setX <- theState
  case vInXwNotInX edgesWithL setX of
    [] -> return setX
    selectedVWs ->
      let wWithLength = getWTuple edgesWithL setX selectedVWs
      in doDj (pure $ liftA2 (++) setX wWithLength)

getWTuple :: [EdgeWithLengh] -> Maybe X -> [EdgeWithLengh] -> Maybe [(Id, Id)]
getWTuple _ Nothing _ = Nothing
getWTuple edgesWithL (Just setX) selectedVWs = do
  (v', w') <- findShortestVW setX selectedVWs
  let vLength = getVlength v' setX
  let vwLength = lengthFor (v', w') edgesWithL
  let wLength = liftA2 (+) vLength vwLength
  fmap (:[]) . sequence $ (w', wLength)

vInXwNotInX :: [EdgeWithLengh] -> Maybe X -> [EdgeWithLengh]
vInXwNotInX _ Nothing = []
vInXwNotInX edges (Just setX) = filter (\(v,w,_) -> v `elem` vsFromX && w `notElem` vsFromX) edges
  where
    vsFromX = fmap fst setX

findShortestVW :: X -> [EdgeWithLengh] -> Maybe (Int, Int)
findShortestVW setX selectedXs = do
  let scoredVWs = fmap (\(v',w',l) -> (v',w', (+ l) <$> getVlength v' setX)) selectedXs
  (v,w,_) <- headOrNothing . sortBy (\(_,_,a) (_,_,b) -> compare a b) $ scoredVWs
  return (v, w)

lengthFor :: (Id, Id) -> [EdgeWithLengh] -> Maybe Int
lengthFor (v', w') = headOrNothing . fmap (\(_,_,l) -> l) . filter (\(v, w, _) -> v == v' && w == w')

headOrNothing :: [a] -> Maybe a
headOrNothing [] = Nothing
headOrNothing (x:_) = Just x

getVlength :: Id -> X -> Maybe Int
getVlength v = headOrNothing . fmap snd . filter ((==v) . fst)

--- O(n*m) = O(n^2)
edgesWithDistances :: Graph -> [EdgeWithLengh]
edgesWithDistances = join . fmap (\(Vertex gid edges) -> transformToEdges gid edges)
  where
    transformToEdges gid = fmap (\(edge, l) -> (gid, edge, l))

--- O(n^2)
graphToEdges :: Graph -> [Edge]
graphToEdges = join . fmap (\(Vertex gid edges) -> transformToEdges gid edges)
  where
    transformToEdges gid = fmap (\(edge, _) -> (gid, edge))


-- dj :: Graph -> Id -> X
-- dj graph gid = evalState (doDj (edgesWithDistances graph) [(gid, 0)]) []

-- doDj :: [EdgeWithLengh] -> X -> DjState
-- doDj edgesWithL setX =
--   case vInXwNotInX edgesWithL setX of
--     [] -> return setX
--     selectedVWs -> do
--       let (v', w') = findShortestVW setX selectedVWs
--       let vLength = getVlength v' setX
--       let vwLength = lengthFor (v', w') edgesWithL
--       let wLength = vLength + vwLength
--       doDj edgesWithL (setX ++ [(w', wLength)])
