sum_puzzle           :: [[Int]] -> Int
sum_puzzle (x:xs)    = single_sum x (maxBound :: Int) 0 + sum_puzzle xs
sum_puzzle []        = 0

single_sum                          :: [Int] -> Int -> Int -> Int
single_sum (x:xs) min max
              | x < min && x > max  = single_sum xs x x
              | x < min             = single_sum xs x max
              | x > max             = single_sum  xs min x
              | otherwise           = single_sum xs min max
single_sum [] min max               = max - min
