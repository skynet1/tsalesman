module Output where

import Types

path2str :: [Node] -> Char -> String
path2str nodes separator = do
    let lines = map (\x -> [separator] ++ (show x)) nodes
    let outStr = foldr (++) "" lines
    drop 1 outStr
