A product sum number N is a number for which there exists a factorisation:
f1 x f2 ... fi = N
Where the sum of the digits is also equal to N. For example, 6 is such a number:
1 x 2 x 3 = 1 + 2 + 3 = 6
Another example is 4:
2 x 2 = 2 + 2

> import Data.List(sort, nub)

Write the predicate:

> isProductSum :: Integral a => a -> Bool
> isProductSum n = any sumsToN factorsets
> -- This works because we can pad factorisations with 1s to obtain a higher sum
> -- and still have the same factorisation 
>   where sumsToN fs = sum fs < n 
>         factorsets = filter (/= [n]) $ factorisations n

> factorisations :: Integral a => a -> [[a]]
> factorisations n = nub $ map sort $ go n 2
>   where go n i
>           | i * 2 > n = [[n]]
>           | i `divides` n =
>             -- the factorisations we get from 'not using' i in addition to
>             -- the ones we get by using it
>             let withI = map (i : ) $ go (n `div` i) i
>                 noI = go n (i + 1)
>             in withI ++ noI
>           | otherwise = go n (i + 1)

> divides :: Integral a => a -> a -> Bool
> divisor `divides` number = number `rem` divisor == 0
