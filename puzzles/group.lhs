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
groupBy predicate = group (\x ys -> x `predicate` (head ys))

Ingen imports, din kode her:
