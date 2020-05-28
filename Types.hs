module Types where

type Node = String 
data Edge = Edge Node Node Double deriving(Show)
data Graph = Graph [Edge] deriving(Show)
