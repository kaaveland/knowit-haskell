module Main (main) where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
  then interact tac
  else mapM_ (open tac) args
  where
    tac = unlines . reverse . lines
    open f file = do 
      contents <- readFile file
      putStr (f contents)
