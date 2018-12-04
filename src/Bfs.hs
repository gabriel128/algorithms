module Bfs where

import Data.List
import Data.Maybe

type Graph = [Vertex]

type VertixIds = [Int]
type Id = Int
type Layer = Int
type Explored = Bool

data Vertex = Vertex Id Explored Layer VertixIds deriving (Show,Eq, Ord)
data Edge = Edge Int Int

data Queue a = Queue [a] [a] deriving Show

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

push :: a -> Queue a -> Queue a
push a (Queue xs ys) = Queue xs (a:ys)

pop :: Queue a -> (Maybe a, Queue a)
pop (Queue [] []) = (Nothing, Queue [] [])
pop (Queue [] ys) = pop (Queue (reverse ys) [])
pop (Queue (x:xs) ys) = (Just x, Queue xs ys)

markAsExplored :: Vertex -> Vertex
markAsExplored (Vertex vid _ layer edges) = Vertex vid True layer edges

setLayer :: Layer -> Vertex -> Vertex
setLayer layer (Vertex vid explored _ edges) = Vertex vid explored (layer + 1) edges

notExplored :: Maybe Vertex -> Bool
notExplored (Just (Vertex _ explored _ _)) = not explored
notExplored Nothing = False

fetchVertex :: Graph -> Id -> Maybe Vertex
fetchVertex graph vid = getFrom vertices
  where
    (vertices, rest) = foldr f ([],[]) graph
    f vert@(Vertex x _ _ _) (left, right) = if x == vid then (vert : left, right) else (left, vert : right)
    getFrom [a] = Just a
    getFrom _ = Nothing

markedVertices :: [Id] -> Layer -> Graph -> Maybe [Vertex]
markedVertices edges layer graph = sequence . (fmap . fmap) (markAsExplored . setLayer layer).
  filter notExplored . fmap (fetchVertex graph) $ edges

getNewVertices :: Maybe [Vertex] -> [Vertex]
getNewVertices (Just vertices) = vertices
getNewVertices Nothing = []

maybeToList' :: Maybe [a] -> [a]
maybeToList' (Just a) = a
maybeToList' Nothing = []

bfs :: Graph -> Id -> Graph
bfs initGraph sid = sort . doBfs initQueue $ initGraph
  where
   (Just s) = fetchVertex initGraph sid
   exploredS = markAsExplored s
   initQueue = Queue [exploredS] []
   doBfs queue graph
     | empty queue = graph
     | otherwise = doBfs newQueue newGraph
     where
      (Just vert@(Vertex _ _ layer edges), queue') = pop queue
      newMarkedVertices = markedVertices edges layer graph
      newQueue = foldr push queue' (getNewVertices newMarkedVertices)
      newVertices = vert : getNewVertices newMarkedVertices
      getId (Vertex vid' _ _ _) = vid'
      newVerticesIds = fmap getId newVertices
      newGraph = newVertices ++ filter (\(Vertex x _ _ _) -> x `notElem` newVerticesIds) graph

simpleDfs :: [[Int]] -> Int -> [Int]
simpleDfs graph i = go graph i []
  where
    go graph i explored
      | i `elem` explored = explored
      | otherwise = foldr (go graph) (i:explored) (graph !! i)

dfs :: Graph -> Id -> Graph
dfs graph initI
     | not . any (\(Vertex vid _ _ _) -> vid == initI) $ graph = []
     | otherwise =  sort . fmap markAsExplored $  go initI []
  where
   go i explored
     | any (\(Vertex vid _ _ _) -> vid == i) explored = explored
     | otherwise = foldr go (getS:explored) (edgesOf getS)
      where
        getS = case fetchVertex graph i of
                 (Just s) -> s
                 Nothing -> undefined
        edgesOf (Vertex _ _ _ edges) = edges


testVert1 = [Vertex 1 False 0 [2, 3],
            Vertex 2 False 0 [1],
            Vertex 3 False 0 [1]]

testVert = [Vertex 1 False 0 [2, 3],
            Vertex 2 False 0 [1, 4],
            Vertex 3 False 0 [1, 4, 5],
            Vertex 8 False 0 [9],
            Vertex 9 False 0 [],
            Vertex 4 False 0 [2, 3, 5, 6],
            Vertex 5 False 0 [3, 4, 6],
            Vertex 6 False 0 [4, 5]]

dtestVert = [Vertex 1 False 0 [2, 3],
             Vertex 2 False 0 [4],
             Vertex 3 False 0 [4, 5],
             Vertex 8 False 0 [],
             Vertex 4 False 0 [5, 6],
             Vertex 5 False 0 [6],
             Vertex 6 False 0 []]

simpleVert :: [[Int]]
simpleVert = [[7], [2, 3], [4], [4, 5], [5, 6], [6], [], [8], []]
