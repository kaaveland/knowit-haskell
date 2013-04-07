{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import Data.List(intercalate)
import System.Exit(exitFailure)
import System.Console.CmdArgs(cmdArgsMode, name, explicit, (&=), help,
                              Data(..), args, helpArg, program, Typeable(..),
                              cmdArgsRun, summary, details)
import Control.Applicative((<*>), pure)
import Control.Monad(mapM, mapM_)

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

wordOpt = False &= help "Show wordcounts"
charOpt = False &= help "Show character counts"
lineOpt = False &= help "Show line counts"
sysin = False &= name "stdin" &= name "s" &= help "Read stdin"
mLineOpt = False &= name "max-line-count" &= name "L"
           &= help "Show max line counts" &= explicit
quietOpt = False &= help "Do not print a total at the end"

wc = cmdArgsMode $
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
     &= details (["Take files from argv or use stdin if none are present."] ++
                 ["-L is incompatible with -l, -c and -w. To read from multiple"] ++
                 ["files in addition to stdin, provide -s. With -s, stdin is"] ++
                 ["always read last. With -q, a summary is not printed as the"] ++
                 ["the last line."])

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

countFile :: FilePath -> IO [Int]
countFile "-" = getContents >>= return . getCount
countFile p = readFile p >>= return . getCount

outputSettings :: Wc -> [Bool]
outputSettings = ([lineCount, wordCount, charCount, maxLines] <*>) . pure

chooseOutput :: Wc -> [Int] -> [Int]
chooseOutput wc counts = map snd selected
  where settings = outputSettings wc
        selected = filter fst $ zip settings counts

formatOutput :: [Int] -> String
formatOutput = intercalate " " . map show

wcStdin :: Wc -> IO ()
wcStdin wc = do counts <- countFile "-"
                let outp = wc `chooseOutput` counts
                putStrLn $ "- " ++ formatOutput outp

printTotal :: Wc -> [[Int]] -> IO ()
printTotal wc xs = if maxLines wc
                   then putStrLn $ "Total " ++ max
                   else putStrLn $ "Total " ++ total
  where total = formatOutput elementWiseSum
        elementWiseSum = foldr1 (zipWith (+)) xs
        max = show $ maximum $ concat xs

runWc :: Wc -> IO ()
runWc wc
  | length (paths wc) == 0 = wcStdin wc
  | otherwise = do
    counts <- mapM countFile (paths wc)
    let withNames = zip (paths wc) counts
    additionally <- if readStdin wc then countFile "-" else return $ repeat 0
    let withNames' = if readStdin wc
                     then withNames ++ [("-", additionally)]
                     else withNames
    let out = map (\(name, counts) -> (name, chooseOutput wc counts)) withNames'
    mapM_ (\(name, counts) -> putStrLn $ name ++ " " ++ (formatOutput counts)) out
    if length out > 1
      then printTotal wc $ map snd out
      else return ()

main :: IO ()
main = do wc <- cmdArgsRun wc
          let validWc = validate wc
          case validWc of
            (Left msg) -> do putStrLn msg
                             exitFailure
            (Right wc') -> runWc wc'
