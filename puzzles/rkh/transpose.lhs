Nøtten består i å skrive en funksjon som gitt er matrise returnerer den transponerte
matrisen.

Den transponerte av en matrise er matrisen som har samme kolonne-vektorer som
rad-vektorene i input-matrisen.

Pseudo-kode for en imparativ løsning:
```
Matrix transpose(Matrix m)
    Matrix transposed = new Matrix()
    transposed.width = m.height
    transposed.height = m.width

    for i = 0 to m.width
        for j = 0 to m.height
            transposed[j][i] = m[i][j]
```
Ikke benytt funksjoner som ikke finnes i Prelude (ingen imports)

> type Row a = [a]
> type Matrix a = [Row a]

Din kode her:

Hjelpefunksjoner:

Denne kan gjettes:
not :: Bool -> Bool
Tom liste?
null :: [a] -> Bool

> some = not . null

Oppfyller alle elementer i lista kriterie?
all :: (a -> Bool) -> [a] -> Bool

Finnes det noe i alle sublister?

> allSome :: [[a]] -> Bool
> allSome = all some

Drop første element i alle sublister:

> tails :: [[a]] -> [[a]]
> tails = map tail

Hent første element i alle sublister:

> heads :: [[a]] -> [a]
> heads = map head

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile henter neste element fra en liste så lenge det oppfyller en condition

iterate :: (a -> a) -> a -> [a]
Repeterer en funksjon, kan skrives f. eks:

> iter f item = let next = f item in item : iter f next

Repeter dropping av første element så lenge alle sublister har elementer

> allTails :: [[a]] -> [[[a]]]
> allTails = takeWhile allSome  . iterate tails

Ser slik ut i praksis:
*Main> allTails [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
[[[1,2,3],[4,5,6],[7,8,9]],
 [[2,3],[5,6],[8,9]],
[[3],[6],[9]]]

Denne kan gjettes...

> allHeads :: [[[a]]] -> [[a]]
> allHeads = map heads

I mål:

> transpose :: [[a]] -> [[a]]
> transpose = allHeads . allTails

*Main> transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
[[1,4,7],
 [2,5,8],
 [3,6,9]]

Kortversjonen er 15 ord...

> transp = map (map head) . takeWhile (all (not . null)) . iterate (map tail)
