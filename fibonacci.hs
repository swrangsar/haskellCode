fibonacci :: (Integral a) => [a]
fibonacci = 0:1:zipWith (+) fibonacci (tail fibonacci)