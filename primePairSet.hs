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






set = [[a,b,c,d,e] | a <- primes',
    let depth1 = filter (isPrimePair a) $ dropWhile (<= a) primes',
    b <- depth1,
    let depth2 = filter (isPrimePair b) $ dropWhile (<= b) depth1,
    c <- depth2,
    let depth3 = filter (isPrimePair c) $ dropWhile (<= c) depth2,
    d <- depth3,
    let depth4 = filter (isPrimePair d) $ dropWhile (<= d) depth3,
    e <- depth4]

