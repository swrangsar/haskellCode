
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
  
                             
primeFactorR :: (Integral a) => a -> Int -> a
primeFactorR n index
    | n==1                  = 0
    | p*p > n               = 1
    | (n `mod` p) == 0      = 1 + (primeFactorR n' (index+1))
    | otherwise             = primeFactorR n (index+1)
    where p         = primes !! index
          n'        = factorOut n p


primeFactorCount :: (Integral a) => a -> a
primeFactorCount n = primeFactorR n 0


has4PrimeFactors :: (Integral a) => a -> Bool
has4PrimeFactors n = (primeFactorCount n == 4)


has4Consec :: (Integral a) => a -> Bool
has4Consec n
    | (not.has4PrimeFactors) n          = False
    | (not.has4PrimeFactors) (n+1)      = False
    | (not.has4PrimeFactors) (n+2)      = False
    | (not.has4PrimeFactors) (n+3)      = False
    | otherwise                         = True




candidates = filter has4Consec $ filter (not.isPrime) [100..]

-- candidates = [(a,b,c) | a <- has4Candidates, b <- tail has4Candidates, c <- tail (tail has4Candidates), (a+1) == b, (b+1) == c]


