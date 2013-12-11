import RabinMiller
import Data.List


isPrime :: (Integral a) => a -> Bool
isPrime = RabinMiller.isRabinMillerPseudoPrime




isPrimePair :: Int -> Int -> Bool
isPrimePair a b = (isPrime x) && (isPrime y)
    where   x = readInt $ show a ++ show b
            y = readInt $ show b ++ show a
            readInt = read:: String -> Int

        
        

primes :: (Integral a) => [a]
primes = filter isPrime (2:[3,5..])


primes' = takeWhile (< 25000) primes





set = [(sum [a,b,c,d,e]) | a <- take 11 primes',
    let depth1 = dropWhile (<= a) primes',
    b <- depth1,
    isPrimePair a b,
    let depth2 = dropWhile (<= b) primes',
    c <- depth2,
    isPrimePair a c,
    isPrimePair b c,
    let depth3 = dropWhile (<= c) primes',
    d <- depth3,
    isPrimePair a d,
    isPrimePair b d,
    isPrimePair c d,
    let depth4 = dropWhile (<= d) primes',
    e <- depth4,
    isPrimePair a e,
    isPrimePair b e,
    isPrimePair c e,
    isPrimePair d e]

