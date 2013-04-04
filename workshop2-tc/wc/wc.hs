module Main(main) where

import System.IO
import System.Environment
import Control.Applicative((<*>))
import Data.List(intercalate)

countingFunctions = [length . lines, length . words, length]

count :: String -> [Int]
count s = countingFunctions <*> [s]

showCount = intercalate " " . map show . count

wc f = f >>= hGetContents >>= return . showCount >>= putStrLn

main = do
  args <- getArgs
  let files = if null args
              then [return stdin]
              else map (flip openFile ReadMode) args
  mapM_ wc files
