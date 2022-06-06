module Second where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x /= 0 = let x1 = abs x in summ x1 0 0
              | x == 0 = ( 0, 1 )
              
summ :: (Integral a, Num b) => a -> b -> a -> (a, b)
summ x acc1 acc2 | x > 0 = summ (x `div` 10) (acc1 + 1) (x `mod` 10 + acc2)
                 | x `div` 10 == 0 = (acc2, acc1)



integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper 0 a iters
    where
    iters = 1000
    h = (b - a) / iters
    helper sum x 0 = sum
    helper sum x n = helper (sum + h * (f x + f (x + h)) / 2) (x + h) (n - 1)