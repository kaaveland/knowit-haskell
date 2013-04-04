Nøtt: Dekomprimer er liste som er 'komprimert' slik at elementene er en
tuple av (Int, a) hvor det *første* elementet beskriver antallet ganger
det *andre* elementet skal forekomme.

uncompress :: [(Int, a)] -> [a]

Slik:
Main*> uncompress [(1, 'f'), (2, 'o'), (1, 'b'), (1, 'a'), (1, 'r')]
"foobar"

Ingen imports, din kode her:


