module Main where

import System.Process
import System.Directory
import System.Posix.User
import System.IO

builtins :: [String]
builtins = ["dc"]

main :: IO ()
main =  shell
          where
            shell :: IO ()
            shell = do
                    userName <- getEffectiveUserName
                    relDir <- getRelativeDir
                    putStr (userName ++ "@HaskShell~> " ++ relDir ++ " ")
                    hFlush stdout
                    command <- getLine
                    if command == "exit" then do
                                                 putStrLn "Goodbye! D:"
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


getRelativeDir :: IO String
getRelativeDir =  do
                  homeDir <- getHomeDirectory
                  curDir <- getCurrentDirectory
                  relDir <- getRelativeDir' homeDir curDir
                  return relDir
                  where
                    getRelativeDir' :: String -> String -> IO FilePath
                    getRelativeDir' [] [] = return "~/"
                    getRelativeDir' [] a = return ("~" ++ a)
                    getRelativeDir' _ [] = getCurrentDirectory
                    getRelativeDir' (x:xs) (y:ys)
                      | x == y = getRelativeDir' xs ys
                      | x /= y = getCurrentDirectory


