import RabinMiller


isPrime :: (Integral a) => a -> Bool
isPrime = RabinMiller.isRabinMillerPseudoPrime


isPrimePair :: (Integral a) => a -> a -> Bool
isPrimePair a b = (isPrime x) && (isPrime y)
    where   x = a*p + b
            y = b*q + a
            p = head $ dropWhile (<b) $ map (10^) [0..]
            q = head $ dropWhile (<a) $ map (10^) [0..]
        
        

primes :: (Integral a) => [a]
primes = filter isPrime [2..]



pairs = [[a,b]| a <- primes, let p = takeWhile (< a) primes, b <- p, isPrimePair a b]