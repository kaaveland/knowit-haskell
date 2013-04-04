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
