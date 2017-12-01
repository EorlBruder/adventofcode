sum_puzzle                    :: [Integer] -> Integer
sum_puzzle (x:(y:xs))
                | x == y      = x + sum_puzzle_rec (y:xs) x
                | otherwise   = sum_puzzle_rec (y:xs) x
sum_puzzle (x:[])             = x

sum_puzzle_rec                :: [Integer] -> Integer -> Integer
sum_puzzle_rec (x:(y:xs)) z
                | x == y      = x + sum_puzzle_rec (y:xs) z
                | otherwise   = sum_puzzle_rec (y:xs) z
sum_puzzle_rec (x:[]) z
                | x == z      = x
                | otherwise   = 0
