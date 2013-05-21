module Comprehensions where

import Control.Monad
import Data.Functor

addOne1 :: Num t => [t] -> [t]
addOne1 l = 
    [x + 1 | x <- l]

addOne2 :: (Functor f, Num b) => f b -> f b
addOne2 l = fmap (+1) l

addOne3 :: (Monad m, Num b) => m b -> m b
addOne3 l = l >>= (\x -> return (x + 1))

addOne4 :: (Monad m, Num r) => m r -> m r
addOne4 l = liftM (+1) l


-- Guard med MonadPlus
addOneToEven1 :: Integral t => [t] -> [t]
addOneToEven1 l = 
    [x + 1 | x <- l, (x `mod` 2) == 0]

addOneToEven2 :: (Integral b, MonadPlus m) => m b -> m b
addOneToEven2 l = do
    x <- l
    guard $ (x `mod` 2) == 0
    return (x + 1)

addOneToEven3 :: (Integral b, MonadPlus m) => m b -> m b
addOneToEven3 l = 
    l >>= (\x -> (guard $ (x `mod` 2) == 0)
      >>  return (1 + x))


-- Kombinere 2
addTwo1 :: Num t => [t] -> [t] -> [t]
addTwo1 l1 l2 = 
    [x + y | x <- l1, y <- l2]

addTwo2 :: (Monad m, Num b) => m b -> m b -> m b
addTwo2 l1 l2 = 
    l1 >>= (\x -> l2 >>= \y -> return $ x + y)

addTwo3 :: (Monad m, Num b) => m b -> m b -> m b
addTwo3 l1 l2 = do
    x <- l1 
    y <- l2 
    return $ x + y

addTwo4 :: (Monad m, Functor m, Num b) => m b -> m b -> m b
addTwo4 l1 l2 = 
    l1 >>= (\x -> (\y -> x + y) <$> l2)
