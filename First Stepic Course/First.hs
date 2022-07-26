
import Data.Char ( isDigit, digitToInt )



lenVec3 :: Floating a => a -> a -> a -> a
lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)
sign :: (Ord a, Num a, Num p) => a -> p
sign x 
    | x < 0 = (-1)
    | x > 0 = (1)
    | otherwise = 0

infixl 7 |-|
x |-| y = abs(x-y)


discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2 