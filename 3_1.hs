solve_puzzle       :: Int -> Int
solve_puzzle x     = (get_spiral_width x `div` 2) + get_spiral_position x

get_spiral_position   :: Int -> Int
get_spiral_position 1 = 0
get_spiral_position x = abs((normalize_spiral x `mod` quadrant_size x) - quadrant_size x `div` 2) 

quadrant_size :: Int -> Int
quadrant_size x = ((get_spiral_width x)^2 - (get_spiral_width x - 2)^2) `div` 4

normalize_spiral :: Int -> Int
normalize_spiral x = x - (get_spiral_width x - 2)^2

get_spiral_width   :: Int -> Int
get_spiral_width x = get_spiral_width_rec x 1

get_spiral_width_rec      :: Int -> Int -> Int
get_spiral_width_rec x y
      | x <= y*y          = y
      | otherwise         = get_spiral_width_rec x (y+2)
