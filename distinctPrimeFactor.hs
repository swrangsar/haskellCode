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
                
primeFactorWorker :: (Integral a) => a -> Int -> [a]
primeFactorWorker n index
    | p > n                 = []
    | (n `mod` p) == 0      = p:primeFactorWorker n' (index+1)
    | otherwise             = primeFactorWorker n' (index+1)
    where p         = primes !! index
          n'        = factorOut n p
          

primeFactors :: (Integral a) => a -> [a]
primeFactors n = primeFactorWorker n 0

distinctPrimeFactorCount :: (Integral a) => a -> Int
distinctPrimeFactorCount n = length $ primeFactors n


has4PrimeFactors :: (Integral a) => a -> Bool
has4PrimeFactors n = 4 == distinctPrimeFactorCount n


has4Consec :: (Integral a) => a -> Bool
has4Consec n
    | (not.has4PrimeFactors) n          = False
    | (not.has4PrimeFactors) (n+1)      = False
    | (not.has4PrimeFactors) (n+2)      = False
    | (not.has4PrimeFactors) (n+3)      = False
    | otherwise                         = True



candidates = filter has4Consec $ filter (not.isPrime) [1..]


