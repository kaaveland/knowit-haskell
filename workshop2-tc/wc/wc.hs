module Main(main) where

import System.IO
import System.Environment
import Control.Applicative((<*>), pure)
import Control.Monad
import Data.List(intercalate)

countingFunctions = [length . lines, length . words, length]

count :: String -> [Int]
count = (countingFunctions <*>) . pure

showCount = intercalate " " . map show . count
wc = (putStrLn =<<) . fmap showCount . (hGetContents =<<)

main = do
  args <- getArgs
  let files = if null args
              then [return stdin]
              else map (flip openFile ReadMode) args
  mapM_ wc files
