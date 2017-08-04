module Main where

import System.Process
import System.Directory
import System.Posix.User

builtins :: [String]
builtins = ["dc"]

main :: IO ()
main =  shell
          where
            shell :: IO ()
            shell = do
                    userName <- getEffectiveUserName
                    putStrLn (userName ++ "@HaskShell->")
                    command <- getLine
                    if command == "exit" then do
                                                 putStrLn "Goodbye!"
                                                 return ()
                                         else do 
                                                 executeLine command
                                                 shell


executeLine :: String -> IO ()
executeLine [] = putStrLn ""
executeLine a 
  | elem command builtins = runBuiltin $ commandFinder a
  | otherwise = do 
                system a
                return ()
    where
    (command, leftoverString) = commandFinder a

commandFinder :: String -> (String, String)
commandFinder string = commandFinder' [] string
  where
    commandFinder' :: String -> String -> (String, String)
    commandFinder' accum []       = (accum, [])
    commandFinder' accum (' ':ys) = (accum, ys)
    commandFinder' accum (y:ys)   =  commandFinder' (y:accum) ys
      
runBuiltin :: (String, String) -> IO ()
runBuiltin (a, b)
  | a == "dc" = shellChangeDirectory b

shellChangeDirectory :: String -> IO ()
shellChangeDirectory [] = do
                          homeDir <- getHomeDirectory
                          setCurrentDirectory homeDir
shellChangeDirectory a  = setCurrentDirectory a
