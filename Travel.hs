module Travel where

import Types
import Data.Tree
import Data.List
import Graph
import Control.Parallel.Strategies
import GHC.Conc

travel :: Graph -> Double -> [Node] -> [Node] -> [(Double, [Node], [Node])]
travel (Graph edges) currentDist startNodes destNodes = do
    let connectedNodes = filter (\(Edge start stop dist) -> start == (startNodes !! 0) ) edges
    let filterNodes node nodes = filter (\x -> x /= node) nodes
    let ret = map (\(Edge _ stop dist) -> (currentDist + dist , (stop : startNodes), (filterNodes stop destNodes))) connectedNodes
    ret

travelTree :: Graph -> Double -> [Node] -> [Node] -> Tree (Double, [Node], [Node])
travelTree graph dist start destNodes = do
    let sub = map (\(dist2, start2, destNodes2) -> travelTree graph dist2 start2 destNodes2 ) (travel graph dist start destNodes)
    let root = Data.Tree.Node {rootLabel = (dist, start, destNodes), subForest = sub}
    root

getPath :: Graph -> Int -> Node -> [Node] -> [Node]
getPath graph numRoutes start destNodes = do
    let l = levels (travelTree graph 0.0 [start] destNodes ) `using` parBuffer numCapabilities rdeepseq
    let samples = ( foldr (++) [] l ) 
    let samples2 = filter (\(_, _, z) -> z == []) samples
    let samples3 = map (\(x, y, _) -> (x, y)) samples2  
    let taken = take numRoutes samples3 `using` parBuffer numCapabilities rseq
    let sorted = sort taken
    let (_, final) = sorted !! 0 in reverse final
