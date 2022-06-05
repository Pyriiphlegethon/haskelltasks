
lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)
sign x 
    | x < 0 = (-1)
    | x > 0 = (1)
    | otherwise = 0
    