{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import System.Exit(exitFailure)
import System.Console.CmdArgs(cmdArgsMode, name, explicit, (&=), help,
                              Data, args, helpArg, program, Typeable,
                              cmdArgsRun, summary, details, Mode, CmdArgs)
import Control.Applicative((<*>), pure)
import Control.Monad(liftM, when)
import Control.Arrow(second)

data Wc = WcProgram
          { paths :: [FilePath]
          , wordCount :: Bool
          , lineCount :: Bool
          , charCount :: Bool
          , maxLines :: Bool
          , readStdin :: Bool
          , quiet :: Bool
          }
        deriving (Show, Data, Typeable)

wordOpt, charOpt, lineOpt, sysin, mLineOpt, quietOpt :: Bool
wordOpt = False &= help "Show wordcounts"
charOpt = False &= help "Show character counts"
lineOpt = False &= help "Show line counts"
sysin = False &= name "stdin" &= name "s" &= help "Read stdin"
mLineOpt = False &= name "max-line-count" &= name "L"
           &= help "Show max line counts" &= explicit
quietOpt = False &= help "Do not print a total at the end"

description :: [String]
description = ["Take files from argv or use stdin if none are present."] ++
              ["-L is incompatible with -l, -c and -w. To read from multiple"] ++
              ["files in addition to stdin, provide -s. With -s, stdin is"] ++
              ["always read last. With -q, a summary is not printed as the"] ++
              ["the last line."]

wcDefault :: Mode (CmdArgs Wc)
wcDefault = cmdArgsMode $
            WcProgram { paths = [] &= args
                      , wordCount = wordOpt
                      , lineCount = lineOpt
                      , charCount = charOpt
                      , maxLines = mLineOpt
                      , readStdin = sysin
                      , quiet = quietOpt
                      }
            &= program "wc" &= helpArg [explicit, name "h", name "help"]
            &= summary "List different counts presented in input files or stdin."
            &= details description

counters :: [String -> Int]
counters = [length . lines, length . words, length, maximum . map length . lines]

validate :: Wc -> Either String Wc
validate wc
  | maxLines wc && or outputsettings =
    Left "-L is incompatible with all of -w -l -c"
  | maxLines wc = Right wc
  | not $ or outputsettings =
    Right $ wc {wordCount = True, lineCount = True, charCount = True}
  | otherwise = Right wc
  where outputsettings = [wordCount, lineCount, charCount] <*> pure wc

getCount :: String -> [Int]
getCount = (counters <*>) . pure

outputSettings :: Wc -> [Bool]
outputSettings = ([lineCount, wordCount, charCount, maxLines] <*>) . pure

chooseOutput :: Wc -> [Int] -> [Int]
chooseOutput wc counts = map snd selected
  where settings = outputSettings wc
        selected = filter fst $ zip settings counts

formatOutput :: [Int] -> String
formatOutput = unwords . map show

fetch :: FilePath -> IO String
fetch "-" = getContents
fetch f = readFile f

countFiles :: [FilePath] -> IO [[Int]]
countFiles = mapM countFile

countFile :: FilePath -> IO [Int]
countFile = liftM getCount . fetch

wcStdin :: Wc -> IO ()
wcStdin wc = do counts <- countFile "-"
                let outp = wc `chooseOutput` counts
                putStrLn $ "- " ++ formatOutput outp

printTotal :: Wc -> [[Int]] -> IO ()
printTotal _ [] = return ()
printTotal wc xs = if maxLines wc
                   then putStrLn $ "Total " ++ maxLine
                   else putStrLn $ "Total " ++ total
  where total = formatOutput elementWiseSum
        elementWiseSum = foldr1 (zipWith (+)) xs
        maxLine = show $ maximum $ concat xs

printFileStat :: (FilePath, [Int]) -> IO ()
printFileStat (path, counts) = putStrLn $ path ++ " " ++ formatOutput counts

runWc :: Wc -> IO ()
runWc wc
  | null (paths wc) = wcStdin wc
  | otherwise = do
    let actualPaths = paths wc ++ [ "-" | readStdin wc ]
    counts <- countFiles actualPaths
    let withNames = zip (paths wc) counts
    let out = map (second $ chooseOutput wc) withNames
    mapM_ printFileStat out
    when (length out > 1) $ printTotal wc $ map snd out

main :: IO ()
main = do wc <- cmdArgsRun wcDefault
          let validWc = validate wc
          case validWc of
            (Left msg) -> do putStrLn msg
                             exitFailure
            (Right wc') -> runWc wc'
