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


set3 = filter (isPrimePair 3) primes
set7 = filter (isPrimePair 7) primes
set109 = filter (isPrimePair 109) primes
set673 = filter (isPrimePair 673) primes
set = intersect set673 $ intersect set109 $ intersect set7 set3
