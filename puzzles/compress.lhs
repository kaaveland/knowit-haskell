Gitt følgende data-type:

> import Data.List

> data Occurs a = Once a | Many Int a deriving (Eq, Ord)

Implementer Show for Occurs slik at enten bare a skrives ut eller a => antall
Eksempel:
*Main> show [Once 3, Many 3 2]
"[3, 3 => 2]"

Gåte: Kan Occurs oppføre seg som en Monad? Hvorfor/hvorfor ikke?
Hint:
(>>=) :: Monad m => m a -> (a -> m b) -> m b

Skriv funksjonen under slik compress returnerer en liste med tupler (antall, element)
for antall er antallet ganger elementet forekommer i rekkefølge. Funksjonen skal
oppføre seg slik at uncompress fra uncompress-nøtta er en "undo" av compress, altså
uncompress . compress = id

> compress :: Eq a => [a] -> [(Int, a)]

Skriv funksjonen compressToOccurs:

> compressToOccurs :: Eq a => [a] -> [Occurs a]

Skriv funksjonen uncompressOccurs:

> uncompressOccurs :: [Occurs a] -> [a]

Implementer Functor for Occurs.
