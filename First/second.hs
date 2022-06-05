module Second where

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x /= 0 = let x1 = abs x in summ x1 0 0
              | x == 0 = ( 0, 1 )
              
summ x acc1 acc2 | x > 0 = summ (x `div` 10) (acc1 + 1) (x `mod` 10 + acc2)
                 | x `div` 10 == 0 = (acc2, acc1)



