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




sets = [[a,b,c,d,e] | a <- primes,
    b <- takeWhile (<a) primes,
    isPrimePair a b,
    c <- takeWhile (<b) primes,
    isPrimePair b c,
    isPrimePair a c,
    d <- takeWhile (<c) primes,
    isPrimePair c d,
    isPrimePair b d,
    isPrimePair a d,
    e <- takeWhile (<d) primes,
    isPrimePair d e,
    isPrimePair c e,
    isPrimePair b e,
    isPrimePair a e]


pairs = sort $ nub $ concat $ take 2000 $ [[a,b]| a <- primes, b <- takeWhile (<a) primes, isPrimePair a b]

triplets = sort $ nub $ concat $ [[a,b,c]| a <- pairs, b <- takeWhile (<a) pairs, isPrimePair a b, c <- takeWhile (<b) pairs, isPrimePair b c, isPrimePair a c]
quartlets = (sort.nub.concat) $ [[a,b,c,d] | a <- triplets, b <- takeWhile (<a) triplets, isPrimePair a b, c <- takeWhile (<b) triplets, isPrimePair b c, isPrimePair a c, d <- takeWhile (<c) triplets, isPrimePair c d, isPrimePair b d, isPrimePair a d]
pentlets = (sort.nub.concat) $ [[a,b,c,d,e]| a <- quartlets, b <- takeWhile (<a) quartlets, isPrimePair a b, c <- takeWhile (<b) quartlets, isPrimePair a c, isPrimePair b c, d <- takeWhile (<c) quartlets, isPrimePair a d, isPrimePair b d, isPrimePair c d, e <- takeWhile (<d) quartlets, isPrimePair a e, isPrimePair b e, isPrimePair c e, isPrimePair d e]

