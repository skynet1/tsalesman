module Travel where

import Types

travel :: Graph -> Double -> Node -> [Node] -> [(Double, Node, [Node])]
travel (Graph edges) currentDist startNode destNodes = do
	let connectedNodes = filter (\(Edge start stop dist) -> start == startNode) edges
	let filterNodes node nodes = filter (\x -> x /= node) nodes
	let ret = map (\(Edge _ stop dist) -> (currentDist + dist , stop, (filterNodes stop destNodes))) connectedNodes
	ret
