
primes :: (Integral a) => [a]
primes = 2:filter isPrime [3,5..]


isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p s -> p*p > n || ((n `mod` p) /= 0 && s))
                True primes
                

isGoldbachForPrime :: (Integral a) => a -> a -> Bool
isGoldbachForPrime n p
    | (mod (n-p) 2) /= 0        = False
    | x == (root*root)          = True
    | otherwise                 = False
    where x     = div (n-p) 2
          root  = (truncate.sqrt.fromIntegral) x


isGoldbach :: (Integral a) => a -> Bool
isGoldbach n = foldr (||) False $ map (isGoldbachForPrime n) $ takeWhile (<n) primes


candidates = filter (not.isGoldbach) $ filter (not.isPrime) [1,3..]
    