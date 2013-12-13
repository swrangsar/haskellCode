

triangles :: (Integral a) => [a]
triangles = map triangle [1..]
    where triangle n = (n * (n+1)) `div` 2



squares :: (Integral a) => [a]
squares = map square [1..]
    where square n = n * n



