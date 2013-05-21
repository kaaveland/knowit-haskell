{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import Data.List
import System.Console.CmdArgs

data PasteOptions = PasteOptions
                  { delimiter :: String
                  , serial :: Bool
                  , files :: [FilePath]
                  }
                  deriving (Show, Eq, Data, Typeable)

pasteDefault :: PasteOptions
pasteDefault = PasteOptions 
               { delimiter = "\t" &= help "reuse characters from LIST instead of TABs"
               , serial = False &= help "paste one file at a time instead of in parallel"
               , files = [] &= args}

description :: [String]
description = 
    [ "Write lines consisting of the sequentially corresponding lines from each"
    , "FILE, separated by TABs, to standard output.  With no FILE, or when FILE"
    , "is -, read standard input."
    , ""
    , "Mandatory arguments to long options are mandatory for short options too."]

pasteMode :: Mode (CmdArgs PasteOptions)
pasteMode = cmdArgsMode $ pasteDefault
            &= program "paste" 
            &= helpArg [explicit, name "h", name "help"]
            &= summary "Approximately like GNU paste"
            &= details description

getFileContent :: String -> IO String
getFileContent "-" = getContents
getFileContent p = readFile p

pasteLines :: PasteOptions -> [String] -> IO ()
pasteLines options text = putStrLn $ intercalate (delimiter options) text

pasteStdin :: PasteOptions -> IO ()
pasteStdin options = do text <- getContents
                        pasteLines options $ lines text


pasteFiles :: PasteOptions -> [FilePath] -> IO ()
pasteFiles options files = do texts <- mapM getFileContent files
                              let textsLines = map lines texts
                              let next = if serial options
                                         then textsLines
                                         else transpose textsLines
                              mapM_ (pasteLines options) next 

main = do options <- cmdArgsRun pasteMode
          case files options of
            [] -> pasteStdin options
            fs -> pasteFiles options fs
