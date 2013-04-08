Tillatte imports:

> import Data.List

Gitt følgende data-type:

> data Occurs a = Once a | Many Int a deriving (Eq, Ord)

Implementer Show for Occurs slik at enten bare a skrives ut eller a => antall

Eksempel:

*Main> show [Once 3, Many 3 2]

"[3, 3 => 2]"

> instance (Show a) => Show (Occurs a) where
>   show occurs = case occurs of
>     Once y -> show y
>     Many c y -> show c ++ " => " ++ show y

Gåte: Kan Occurs oppføre seg som en Monad? Hvorfor/hvorfor ikke?
Hint:

`(>>=) :: Monad m => m a -> (a -> m b) -> m b`

`return :: Monad m => a -> m a`

Nei. Vi kan vise hvorfor med å implementere:

> instance Monad Occurs where
>   return = Once -- Naturlige konstruktøren, ellers må vi "gjette" et antall
>   (Once x) >>= f = f x -- Husk at f :: a -> Occurs a her
>   (Many _ x) >>= f = f x

Problemet her er at vi "mister" informasjon ved bruk av >>= !
Right identity for monader sier at:

`m >>= return = m`

Her kan vi enkelt lage et mot-eksempel:

`*Main> let x = Many 3 "hei" in (x >>= return) == x`

Dette skjer fordi >>= "skreller" vekk Many-konstruktøren og return putter in i
Once konstruktøren. Pga. av typen til >>= kan vi ikke lage en konstruktør som
adlyder Right identity - funksjonen man binder med har ingen måte å "huske" hvilken
konstruktør som ble brukt!

Skriv funksjonen under slik compress returnerer en liste med tupler (antall, element)
for antall er antallet ganger elementet forekommer i rekkefølge. Funksjonen skal
oppføre seg slik at uncompress fra uncompress-nøtta er en "undo" av compress, altså
uncompress . compress = id

> compress :: Eq a => [a] -> [(Int, a)]
> compress = map count . groupBy (==)
>   where count xs = (length xs, head xs)

Skriv funksjonen compressToOccurs:

> compressToOccurs :: Eq a => [a] -> [Occurs a]
> compressToOccurs = map occurs . compress
>   where occurs (1, x) = Once x
>         occurs (c, x) = Many c x

Skriv funksjonen uncompressOccurs:

> uncompress :: [(Int, a)] -> [a]
> uncompress = concatMap (uncurry replicate)

> uncompressOccurs :: [Occurs a] -> [a]
> uncompressOccurs = concatMap unoccur
>   where unoccur (Once x) = [x]
>         unoccur (Many c x) = replicate c x

Implementer Functor for Occurs.

Her finnes det bare en fornuftig implementasjon:

> instance Functor Occurs where
>   fmap f (Once a) = Once $ f a
>   fmap f (Many c a) = Many c $ f a
