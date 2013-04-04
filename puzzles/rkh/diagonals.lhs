Nøtten består i å implementere en funksjon som henter ut en liste av
all diagonalene i en matrise.

Tillatte imports:

> import Data.List

Matrise-type:

> type Row a = [a]
> type Matrix a = [Row a]

Visualisering:

> showMatrix :: (Show a) => Matrix a -> String
> showMatrix = intercalate "\n" . map show
> printMatrix :: (Show a) => Matrix a -> IO ()
> printMatrix = putStrLn . showMatrix

Testdata:

> matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: Matrix Int

Diagonalene i testmatrisen:

> testDiagonals = [[1,5,9],[2,6],[3],[4,8],[7],[3,5,7],[2,4],[1],[6,8],[9]]

Foreslått utgangspunkt:

> mainDiagonal = zipWith drop [0..]

Din kode her:

Et par hendige funksjoner man kan benytte seg av:

intercalate :: [a] -> [[a]] -> [a] er som en string-join
transpose :: [[a]] -> [[a]] - transponerer en matrise

(.) :: (b -> c) -> (a -> b) -> a -> c er komposisjon av funksjoner (tenk unix-pipes)

drop :: Int -> [a] -> [a] fjerner Int elementer fra starten av listen
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] kombinerer to lister (elementvis) med en funksjon av
to argumenter.

Vi hadde zipWith (flip (!!)) [0..] for å hente hoved-diagonalen fra venstre
Vi kan hente hele øvre triangel med denne:

> upperTriangle = zipWith drop [0..]

Eksempel:
*Main> printMatrix $ upperTriangle matrix
[1,2,3]
[5,6]
[9]

Kolonnene her er diagonaler. Transpose gir oss kolonner som rader:
*Main> printMatrix $ transpose $ upperTriangle matrix
[1,5,9]
[2,6]
[3]

For å hente ut høyre-venstre diagonaler må vi speile matrisen:

> mirrorOf = map reverse

*Main> printMatrix $ upperTriangle $ mirrorOf matrix
[3,2,1]
[5,4]
[7]

Så transponerer vi igjen for å hente ut diagonalene

Så trenger vi en måte å hente ut det nedre triangelet på.

take :: Int -> [a] -> [a] henter ut Int elementer fra starten av listen

> lowerTriangle = zipWith take [0..]

Da får vi:
*Main> printMatrix $ lowerTriangle matrix
[]
[4]
[7,8]

For at vi skal få diagonaler her må vi speile igjen og så transponere:
*Main> printMatrix $ mirrorOf $ lowerTriangle matrix
[]
[4]
[8,7]

> leftDiagonals m = concatMap transpose [upper, lower]
>   where upper = upperTriangle m
>         lower = mirrorOf $ lowerTriangle m

*Main> leftDiagonals matrix
[[1,5,9],[2,6],[3],[4,8],[7]]

> rightDiagonals = leftDiagonals . mirrorOf
> diagonals m = leftDiagonals m ++ rightDiagonals m

*Main> allDiagonals matrix
[[1,5,9],[2,6],[3],[4,8],[7],[3,5,7],[2,4],[1],[6,8],[9]]
