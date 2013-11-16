import Data.List
import Data.Ord


primes :: (Integral a) => [a]
primes = 2:filter isPrime [3,5..]


isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p s -> p*p > n || ((n `mod` p) /= 0 && s))
                True primes


primeList n = dropWhile (<= n) $ takeWhile (<999998) primes
primeLists = map primeList $ takeWhile (<999998) primes

addLength :: (Integral a) => (a,Int) -> a -> (a,Int)
addLength (a,c) b = (a+b,c+1)


maxPrimeListSum l = maximumBy (\(a,b) (c,d) -> compare a c) $ filter (\(a,b) -> isPrime a)  $ takeWhile (\(a,b) -> a < 999998) $ scanl addLength (0,0) l

maxConsecPrimeSums = filter (\(a,b) -> b > 500) $ map maxPrimeListSum primeLists

reqdAnswer = maximumBy (\(a,b) (c,d) -> compare b d) maxConsecPrimeSums