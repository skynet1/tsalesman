module Main where

import Types
import Graph
import Travel
import Output
import System.Environment
import Data.List.Split
import Data.List
import System.IO

main :: IO ()
main = do
    args <- getArgs
    inputStr <- getContents
    hSetBuffering stdout (BlockBuffering Nothing)
    let graph = getGraph inputStr ((args !! 1) !! 0)
    let start = (args !! 2)
    let nodes = endBy "," (args !! 0)
    let numRoutes = 10000
    let path = getPath graph numRoutes start nodes
    let outputStr = path2str path ((args !! 1) !! 0)
    putStr outputStr

