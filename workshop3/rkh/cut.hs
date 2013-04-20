{-# LANGUAGE DeriveDataTypeable #-}
module Main(main) where

import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.Maybe
import Control.Monad
import System.Console.CmdArgs
import System.Exit

uncat :: [a] -> [[a]]
uncat = map return

spanL :: ([a] -> Bool) -> [a] -> ([a], [a])
spanL _ [] = ([], [])
spanL fn xs = go [] fn xs
  where go ac _ [] = (reverse ac, [])
        go ac fn xss@(x:xs)
          | fn xss = (reverse ac, xss)
          | otherwise = go (x : ac) fn xs

splitList :: Eq a => [a] -> [a] -> [[a]]
splitList [] xs = uncat xs
splitList _ [] = []
splitList p xs
  | p `isPrefixOf` xs = splitList p rest
  | otherwise = this : splitList p next
  where rest = drop (length p) xs
        (this, next) = spanL (p `isPrefixOf`) xs

data Slice = From Int
           | To Int
           | One Int
           | Range Int Int
           | Delete Int
           | Unrange Int Int
           deriving (Show, Eq)

slice :: Slice -> [a] -> [a]
slice (From x) = drop x
slice (To x) = take x
slice (One x) = \l -> [l !! x | x < length l]
slice (Range low hi) = take (hi - low) . drop low
slice (Delete i) = map snd . filter (\(ix, _) -> ix /= i) . zip [0..]
slice (Unrange low hi) = map snd . filter (\(ix, _) -> ix < low || ix > hi) . zip [0..]

applySlices :: [Slice] -> [a] -> [a]
applySlices slices list = concatMap apply slices
  where apply s = slice s list

complementSlices :: [Slice] -> [a] -> [a]
complementSlices slices = applySlices (map complement slices)
  where complement (From i) = To i
        complement (To i) = From i
        complement (One i) = Delete i
        complement (Range lo hi) = Unrange lo hi
        complement x = x

parseRange :: Parser Slice
parseRange = do low <- many1 digit
                _ <- char '-'
                hi <- many1 digit
                return (Range (read low - 1) (read hi - 1))

parseTo :: Parser Slice
parseTo = do _ <- char '-'
             hi <- many1 digit
             return (To $ read hi - 1)

parseFrom :: Parser Slice
parseFrom = do low <- many1 digit
               _ <- char '-'
               return (From $ read low - 1)

parseOne :: Parser Slice
parseOne = do index <- many1 digit
              return (One $ read index - 1)

parseSlice :: Parser Slice
parseSlice = try parseRange <|> try parseTo <|> try parseFrom <|> parseOne

parseSlices :: Parser [Slice]
parseSlices = parseSlice `sepBy1` char ','

cont :: Slice -> Int
cont (One i) = i
cont (To i) = i
cont (From i) = i
cont (Range i _) = i
cont (Unrange i _) = i
cont (Delete i) = i

validateSlice :: Slice -> Either String Slice
validateSlice slice@(Range lo hi)
  | lo >= hi = Left "Invalid slice, start should be before end"
  | lo < 0 = Left "Invalid slice, cuts are 1-indexed"
  | otherwise = Right slice
validateSlice slice
  | cont slice < 0 = Left "Invalid slice, cuts are 1-indexed"
  | otherwise = Right slice

makeSlices :: String -> Either String [Slice]
makeSlices fmt = case parse parseSlices "-c | -f" fmt of
  Left err -> Left (show err)
  Right slices -> validateSlices slices
                    
validateSlices :: [Slice] -> Either String [Slice]
validateSlices [] = Right []
validateSlices (s:ss) = do v <- validateSlice s
                           vs <- validateSlices ss
                           return (v:vs)

makeValidatedSlices :: String -> Either String [Slice]
makeValidatedSlices fmt = makeSlices fmt >>= validateSlices

data CutOptions = CutOptions
                  { fields :: String
                  , delimiter :: String
                  , chars :: String
                  , complement :: Bool
                  , files :: [FilePath]
                  , onlyDelimited :: Bool
                  , outputDelimiter :: String}
                  deriving (Show, Eq, Data, Typeable)

hasContent :: [a] -> Bool
hasContent = not . null

validateCut :: CutOptions -> Either String [Slice]
validateCut opts
  | hasContent (fields opts) && hasContent(chars opts) =
    Left "Only one of --fields --chars may be set"
  | null (fields opts) && null (chars opts) =
    Left "One of --fields --chars must be set"
  | otherwise = makeValidatedSlices (pick fields chars)
  where pick f g = if hasContent $ f opts then f opts else g opts

doCutLine :: CutOptions -> [Slice] -> String -> Maybe String
doCutLine opts slices line = splice $ select $ split line
  where split = splitList (delimiter opts)
        splice l
          | (delim `isInfixOf` line) && onlyDelimited opts = Nothing
          | otherwise = Just $ intercalate outdelim l
        select = (if complement opts then complementSlices else applySlices) slices
        delim = delimiter opts
        outdelim = outputDelimiter opts

doCutLines :: CutOptions -> [Slice] -> String -> String
doCutLines opts slices = unlines . mapMaybe (doCutLine opts slices) . lines

getFileContent :: String -> IO String
getFileContent "-" = getContents
getFileContent p = readFile p

defaultOptions :: CutOptions
defaultOptions =
  CutOptions { fields = "" &= help "spec(,spec)*"
             , delimiter = "" &= help "delimit by this string"
             , chars = "" &= help "spec(,spec)*" &= name "c"
             , complement = False &= help "Complement chosen fields or chars"
             , files = [] &= args
             , onlyDelimited = False &= help "Select only lines with a delimiter"
                               &= explicit &= name "only-delimited"
             , outputDelimiter = "" &= help "Use this delimiter to splice chosen fields"
                                 &= explicit &= name "o" &= name "output-delimiter"
             }

description :: [String]
description = [ "Remove sections from each line of input files."
              ,  "One of --chars --fields must be given. Separates lines using"
              , "--delimiter (defaults to TAB). Splice chosen fields using"
              , "--outputdelimiter when printing result. This defaults to --delimiter"
              , "when using --fields or the empty string when using --chars."
              , "With --onlyDelimited, prints only lines that contain delimiter."]

cutDefault :: Mode (CmdArgs CutOptions)
cutDefault = cmdArgsMode $ defaultOptions
             &= program "cut" &= helpArg [explicit, name "h", name "help"]
             &= summary "Approximately like unix cut"
             &= details description

chain :: [CutOptions -> CutOptions] -> CutOptions -> CutOptions
chain = flip (foldr id)

postProcess :: CutOptions -> CutOptions
postProcess = chain [setinDelim, setoutDelim, addStdin]
  where setoutDelim opts
          | hasContent $ outputDelimiter opts = opts
          | hasContent $ chars opts = opts
          | otherwise = opts { outputDelimiter = delimiter opts }
        addStdin opts
          | hasContent $ files opts = opts
          | otherwise = opts { files = ["-"] }
        setinDelim opts
          | hasContent (delimiter opts) = opts
          | hasContent (chars opts) = opts { delimiter = "" }
          | otherwise = opts { delimiter = "\t" }
                                      
runCut :: CutOptions -> [Slice] -> IO ()
runCut opts slices = forM_ (files opts) cutFile
  where cutFile path = do content <- getFileContent path
                          putStr (doCutLines opts slices content)

main :: IO ()
main = do cut <- cmdArgsRun cutDefault
          let slices = validateCut cut
          case slices of
            Left err -> do putStrLn err
                           exitFailure
            Right ss -> runCut (postProcess cut) ss
