module Travel where

import Types
import Data.Tree
import Graph

travel :: Graph -> Double -> Node -> [Node] -> [(Double, Node, [Node])]
travel (Graph edges) currentDist startNode destNodes = do
    let connectedNodes = filter (\(Edge start stop dist) -> start == startNode) edges
    let filterNodes node nodes = filter (\x -> x /= node) nodes
    let ret = map (\(Edge _ stop dist) -> (currentDist + dist , stop, (filterNodes stop destNodes))) connectedNodes
    ret

travelTree :: Graph -> Double -> Node -> [Node] -> Tree (Double, Node, [Node])
travelTree graph dist start destNodes = do
    let sub = map (\(dist2, start2, destNodes2) -> travelTree graph dist2 start2 destNodes2 ) (travel graph dist start destNodes)
    let root = Data.Tree.Node {rootLabel = (dist, start, destNodes), subForest = sub}
    root
