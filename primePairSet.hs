import RabinMiller
import Data.List

isPrime :: (Integral a) => a -> Bool
isPrime = RabinMiller.isRabinMillerPseudoPrime


isPrimePair :: (Integral a) => a -> a -> Bool
isPrimePair a b = (isPrime x) && (isPrime y)
    where   x = a*p + b
            y = b*q + a
            p = head $ dropWhile (<b) $ map (10^) [0..]
            q = head $ dropWhile (<a) $ map (10^) [0..]
        
        

primes :: (Integral a) => [a]
primes = filter isPrime (2:[3,5..])

primes' = takeWhile (< 10000) primes

pset p = filter (isPrimePair p) primes'



set = [[a,b,c,d,e]| a <- primes', 
    b <- (takeWhile (< a) primes'),
    isPrimePair a b,
    let depth1 = intersect (pset a) (pset b),
    c <- depth1,
    d <- (takeWhile (< c) depth1),
    isPrimePair c d,
    let depth2 = intersect (filter (isPrimePair c) depth1) (filter (isPrimePair d) depth1),
    e <- depth2]

