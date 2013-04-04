Nøtt: Skriv group hvor

group :: (a -> [a] -> Bool) -> [a] -> [[a]]

group er en generalisering av groupBy :: (a -> a -> Bool) -> [a] -> [[a]] som
fungerer slik:
*Main> groupBy (==) [1, 1, 2, 3]
[[1,1],[2],[3]]

group tar en predikat-funksjon som tar en en kandidat til å bli med i gruppen
og gruppen til nå og en liste den skal gruppere.

Den kan for eksempel benyttes til å lage funksjonen chunks slik:
Main*> let chunks size = group (\_ g -> length g < size)
Main*> chunks 3 [1..10]
[[1,2,3],[4,5,6],[7,8,9],[10]]]

Og groupBy slik:
groupBy predicate = group (\x y -> x `predicate` y)

Ingen imports, din kode her:

> group :: (a -> [a] -> Bool) -> [a] -> [[a]]
> group predicate xs = loop xs []
>   where loop [] [] = [] -- Ikke flere elementer, ingen aktiv gruppe
>         loop [] y = [reverse y] -- Ingen flere elementer, men aktiv gruppe
>         loop (x:xs) [] = loop xs [x] -- Anta sant predikat for tom liste
>         loop (x:xs) ys
>           | predicate x ys = loop xs (x : ys) -- x hører hjemme med ys
>           | otherwise = [reverse ys] ++ loop xs [x]

> chunks :: Int -> [a] -> [[a]]
> chunks n = group (\_ group -> length group == n)

Denne redefinerer funksjon fra Prelude, så tuller med navnet

> groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
> groupBy' predicate = group (\x ys -> (head ys) `predicate` x)
