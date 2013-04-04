module Main (main) where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
  then interact prettyPrint
  else do
    mapM_ open args
  where
    open file = do 
      c <- readFile file
      putStr $ prettyPrint c ++ " " ++ file
    countLines = show . length . lines
    countWords = show . length . words
    countChars = show . length
    prettyPrint s = "\t" ++ countLines s ++ "\t" ++ countWords s ++ "\t" ++ countChars s ++ "\n"
