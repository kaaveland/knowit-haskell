A product sum number N is a number for which there exists a factorisation:
f1 x f2 ... fi = N
Where the sum of the digits is also equal to N. For example, 6 is such a number:
1 x 2 x 3 = 1 + 2 + 3 = 6
Another example is 4:
2 x 2 = 2 + 2

Write the predicate:

> isProductSum :: Integral a => a -> Bool
> isProductSum = undefined

That is True when a number is a product sum number and otherwise false.
