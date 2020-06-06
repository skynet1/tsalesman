{-# LANGUAGE InstanceSigs #-}
module Types where

import Control.Parallel.Strategies
import Control.DeepSeq

type Node = String 
data Edge = Edge Node Node Double deriving(Show)
data Graph = Graph [Edge] deriving(Show)

instance NFData Edge where
    rnf :: Edge -> ()
    rnf (Edge n1 n2 d) = rnf n1 `seq` rnf n2 `seq` rnf d
