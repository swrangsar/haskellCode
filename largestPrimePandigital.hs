import Data.List



primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3,5..]


isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes


digitsOf :: (Integral a) => a -> [a]
digitsOf x
    | quot == 0 = x:[]
    | otherwise	= rem:(digitsOf quot)
    where quot = x `div` 10
          rem  = x `mod` 10


      
      
{-- added on oct 24th 2013 --}

isPandigital :: (Integral a) => a -> Bool
isPandigital num
    | length x >= 8  = False
    | check         = True
    | otherwise     = False
    where check = (sum x == floor (l * (l+1) / 2)) && (product x == product [1..len])
          x     = digitsOf num
          l     = fromIntegral $ length x
          len   = floor $ l

pandigitalPrimes :: (Integral a) => [a]
pandigitalPrimes = filter isPrime $ filter isPandigital $ [7777773,7777771..23]




largestPrimePandigital :: (Integral a) => a
largestPrimePandigital = pandigitalPrimes !! 0

{--note solution is 7652413, 7642513, ...--}