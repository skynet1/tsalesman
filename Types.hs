module Types where

type Node = String
data Edge = Edge Node Node Double
data Graph = Graph [Edge]
