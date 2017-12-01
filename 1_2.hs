sum_puzzle                    :: [Integer] -> Integer
sum_puzzle x                  = sum_puzzle_rec(split_list(x)) * 2

sum_puzzle_rec                :: ([Integer],[Integer]) -> Integer
sum_puzzle_rec (x:xs,y:ys)
                | x == y      = x + sum_puzzle_rec (xs,ys)
                | otherwise   = sum_puzzle_rec (xs,ys)
sum_puzzle_rec ([],[])        = 0

split_list                  :: [Integer] -> ([Integer],[Integer])
split_list x                = split_list_rec x ([],[]) (length(x) `div` 2)

split_list_rec                    :: [Integer] -> ([Integer],[Integer]) -> Int -> ([Integer],[Integer])
split_list_rec (x:xs) (f, s) count
                | count > 0       = split_list_rec xs (x:f,s) (count - 1)
                | otherwise       = split_list_rec xs (f,x:s) (count - 1)
split_list_rec [] (x1, y1) _      = (x1, y1)
