Lag en funksjon

doCoinExchange :: [Int] -> gitt et sett mynter [c1, c2, ..., ck]

Int -> et tall n
[Int] -> returnerer den kombinasjonen av mynter som summerer til n

Eksempel:
```haskell
*Main> doCoinExchange [1, 5, 10] 13
[10,1,1,1]
```

> import Data.List

Din kode her:

uncat pakker inn elementer i lister, slik at man kan ta kryss-produkt

> uncat :: [a] -> [[a]]
> uncat = map return

generate genererer resultatlisten av å etter tur conse hvert element
i listen som er første argument til alle listene som er gitt i andre
argument.

> generate :: [a] -> [[a]] -> [[a]]
> generate = (=<<) . flip (map . flip (:))

combinations gir en uendelig liste som inneholder alle(!) kombinasjoner
man kan lage av argumentet som er oppgitt. De returneres i stigende
rekkefølge etter lengde.

> combinations :: [a] -> [[a]]
> combinations currency = uncat currency ++ generate currency next
>   where next = combinations currency

coinExchange er alle måtene å veksle på, gitt et sett mynter.

> coinExchange :: Num a => [a] -> [(a, [a])]
> coinExchange = map (\coins -> (sum coins, coins)) . combinations

da trenger vi bare å gjøre en lookup inn i en coinExchange for å
løse problemet.

> doCoinExchange :: [Int] -> Int -> [Int]
> doCoinExchange currency n = unjust $ lookup n $ coinExchange currency
>   where unjust (Just x) = x
