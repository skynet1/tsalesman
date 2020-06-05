module Main where

import Types
import Graph
import Travel
import Output
import System.Environment
import System.Console.GetOpt
import Data.List.Split
import Data.List
import System.IO

data Option = Delimiter Char | StartNode Node | Destinations [Node]

options :: [OptDescr Option]
options = [
    Option ['d'] ["delimiter"] (ReqArg (\(x :_) -> Delimiter x  ) ":") "Set the delimiter between node names, and distances",
    Option ['s'] ["start"] (ReqArg (\x -> StartNode x) "s0") "Set the start node.",
    Option ['n'] ["nodes"] (ReqArg (\x -> Destinations (endBy "," x)) "s0") "Comma seperated list of nodes to visit." ]

getDelim :: [Option] -> Char
getDelim [] = ':'
getDelim ((Delimiter c) : _) = c
getDelim (_ : rest) = getDelim rest

getStartNode :: [Option] -> Node
getStartNode [] = "s0"
getStartNode ((StartNode x) : _) = x
getStartNode (_ : rest) = getStartNode rest

getDests :: [Option] -> [Node]
getDests [] = ["s0"]
getDests ((Destinations x) : _) = x
getDests (_ : rest) = getDests rest

main :: IO ()
main = do
    args <- getArgs
    inputStr <- getContents
    let (opts, _, _) = getOpt RequireOrder options args
    hSetBuffering stdout (BlockBuffering Nothing)
    let delim = getDelim opts
    let graph = getGraph inputStr delim
    let start = getStartNode opts
    let nodes = getDests opts
    let numRoutes = 10000000
    let path = getPath graph numRoutes start nodes
    let outputStr = path2str path delim
    putStr outputStr

