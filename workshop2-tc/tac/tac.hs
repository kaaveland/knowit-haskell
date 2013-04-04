module Main (main) where

import System.IO
import System.Environment

main = do
    files <- getArgs
    let handles = if null files 
        then [return stdin] 
        else map (flip openFile ReadMode) files
    mapM_ tac handles

reverseLines :: String -> String
reverseLines = unlines . reverse . lines

tac :: IO Handle -> IO ()
tac handle = handle >>= hGetContents >>= return . reverseLines >>= putStr
