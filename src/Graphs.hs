module Graphs where

-- import Control.Lens
import System.Random
import Data.List

type VertixIds = [Int]
type Id = Int
type Graph = [Vertex]

data Vertex = Vertex Id VertixIds deriving Show
data Edge = Edge Int Int

toVertices :: [[Int]] -> [Vertex]
toVertices = fmap convertRow
  where
    convertRow [] = Vertex 0 []
    convertRow (x:xs) = Vertex x xs

pickRandom :: RandomGen b => [Vertex] -> b -> (Vertex, Int, b)
pickRandom vertices seed = (vertex, edge, seed'')
  where
    (i, seed') = randomR (0, length vertices - 1) seed
    (vertex@(Vertex _ edges)) = vertices !! i
    (i', seed'') = randomR (0, length edges - 1) seed'
    edge = edges !! i'

findVertex :: Int -> [Vertex] -> Maybe Vertex
findVertex i xs =
  case findV of
    [] -> Nothing
    (x:_) -> Just x
  where
    findV = filter predicate xs
    predicate (Vertex n _) =  n == i

fixReferenceInEdges :: Int -> Int -> [Vertex] -> [Vertex]
fixReferenceInEdges i newi = foldr reducer []
  where
    cleanEdges xs = (fmap (const newi) . filter (==i) $ xs) ++ (filter (/=i) xs)
    reducer (Vertex vid edges) acc = Vertex vid (cleanEdges edges) : acc

contract :: RandomGen a => [Vertex] -> a -> [Vertex]
contract [] _ = []
contract [_] _ = undefined
contract [a,b] _ = [a,b]
contract vertices seed = contract finalVertices seed'
  where
    (Vertex vid edges, otherVertexId, seed') = pickRandom vertices seed
    (Just (Vertex _ edges')) = findVertex otherVertexId vertices
    restVertices = filter (\(Vertex n' _) -> n' /= otherVertexId && n' /= vid) vertices
    mergedEdges = nub . filter (/= vid) $ (edges' ++ edges)
    finalVertices = fixReferenceInEdges otherVertexId vid $ Vertex vid mergedEdges : restVertices

vertTest = [Vertex 1 [2, 1], Vertex 2 [1, 3, 4], Vertex 3 [2], Vertex 4 [2]]

main :: IO ()
main = do
  content <- readFile "./src/kargerMinCut.txt"
  let colRows = (fmap (fmap read . words) . lines $ content) :: [[Int]]
  let vertices = toVertices colRows
  seed <- newStdGen
  -- print (filter (\(Vertex n _) -> n == 130) vertices)
  print (contract vertices seed)
  -- print (contract vertTest seed)
