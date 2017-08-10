module Utils
  where

import System.IO


commandFinder :: String -> (String, String)
commandFinder = commandFinder' []
  where
    commandFinder' :: String -> String -> (String, String)
    commandFinder' [] (' ':ys) = commandFinder' []  ys
    commandFinder' accum []       = (reverse accum, [])
    commandFinder' accum (' ':ys) = (reverse accum, ys)
    commandFinder' accum (y:ys)   =  commandFinder' (y:accum) ys
