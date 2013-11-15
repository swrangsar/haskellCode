
primes :: (Integral a) => [a]
primes = 2:filter isPrime [3,5..]


isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p s -> p*p > n || ((n `mod` p) /= 0 && s))
                True primes


factorOut :: (Integral a) => a -> a -> a               
factorOut n p
    | (mod n p) == 0    = factorOut n' p
    | otherwise         = n
    where n' = div n p
  
                             
primeFactorR :: (Integral a) => a -> Int -> a -> a
primeFactorR n index
    | p > n || count > 5    = 0
    | (n `mod` p) == 0      = 1 + (primeFactorR n' (index+1)) 
    | otherwise             = primeFactorR n (index+1)
    where p         = primes !! index
          n'        = factorOut n p
          

primeFactors :: (Integral a) => a -> [a]
primeFactors n = primeFactorR n 0 0


primeFactorCount :: (Integral a) => a -> Int
primeFactorCount n = length $ primeFactors n


has4PrimeFactors :: (Integral a) => a -> Bool
has4PrimeFactors n = (primeFactorCount n == 4)


has4Consec :: (Integral a) => a -> Bool
has4Consec n
    | (not.has4PrimeFactors) n          = False
    | (not.has4PrimeFactors) (n+1)      = False
    | (not.has4PrimeFactors) (n+2)      = False
    | (not.has4PrimeFactors) (n+3)      = False
    | otherwise                         = True


{--
primeFactorCount :: (Integral a) => a -> Int
primeFactorCount n = length $ filter (== 0) $ map (mod n) $ takeWhile (\p -> 2*p <= n) primes
--}

candidates = filter has4Consec $ filter (not.isPrime) [100..]


