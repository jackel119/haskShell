import System.IO
import Data.Maybe
import qualified Data.Text as T

type Line =  (String, String)

getConfigLine :: IO [Line]
getConfigLine = processLines ".hshrc" 

readLines :: FilePath -> IO [String]
readLines path = do
                    contents <- readFile path
                    return (lines contents)

processLines :: FilePath -> IO [Line]
processLines path = do
                        lines <- readLines path
                        return $ filter notEmpty $ map commandFinder lines

notEmpty :: Line -> Bool
notEmpty ("","") = False
notEmpty (a,b)
  | a == "" && all (==' ') b = False
  | otherwise = True

commandFinder :: String -> (String, String)
commandFinder = commandFinder' []
  where
    commandFinder' :: String -> String -> (String, String)
    commandFinder' accum []       = (reverse accum, [])
    commandFinder' accum (' ':ys) = (reverse accum, ys)
    commandFinder' accum (y:ys)   =  commandFinder' (y:accum) ys
