module Main (
    main, 
    Equal)
where

data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where
    show Red = appendLight "Red"
    show Yellow = appendLight "Yellow"
    show Green = appendLight "Green"

appendLight a = a ++ " light"

instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False


class (Show a) => Equal a where
    eq :: a -> a -> Bool
    eq a b = show a == show b
    neq :: a -> a -> Bool
    neq a b = not(eq a b)

instance Equal TrafficLight 

instance (Equal m) => Equal (Maybe m) where  
    Just x `eq` Just y = x `eq` y  
    Nothing `eq` Nothing = True  
    _ `eq` _ = False  

instance Equal Int where
    eq a b = a == b


instance Ord TrafficLight where
    compare a b | a == b = EQ
    compare Red _ = GT
    compare Yellow _ = GT
    compare _ _ = LT
            

showAndCheckEq :: Equal a => a -> a -> ([Char], Bool)
showAndCheckEq t1 t2 = let str = (show t1) ++ " is equal to " ++ (show t2) ++ "? "
                           bool = t1 `eq`t2
                        in (str, bool)


main = do
    let (str, bool) = showAndCheckEq Red Red
    putStrLn $ str ++ " - " ++ (show bool)
    let (str, bool) = showAndCheckEq Red Yellow
    putStrLn $ str ++ " - " ++ (show bool)
    let (str, bool) = showAndCheckEq Red Green
    putStrLn $ str ++ " - " ++ (show bool)

