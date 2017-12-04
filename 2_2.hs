sum_puzzle           :: [[Int]] -> Int
sum_puzzle (x:xs)    = find_division x + sum_puzzle xs
sum_puzzle []        = 0

find_division                           :: [Int] -> Int
find_division (x:xs)
            | (find_divisor x xs) > 0   = find_divisor x xs
            | otherwise                 = find_division xs
find_division []                        = 0

find_divisor                            :: Int -> [Int] -> Int
find_divisor x (y:ys)
            | x > y && (x `mod` y) == 0   = x `div` y
            | x <= y && (y `mod` x) == 0  = y `div` x
            | otherwise                 = find_divisor x ys
find_divisor _ []                       = -1
