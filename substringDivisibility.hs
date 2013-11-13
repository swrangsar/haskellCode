import Data.List


primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3,5..]

isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes

{-- got the prime list --}

num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0 = x:[]
    | otherwise	= rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10

isPandigital :: (Integral a) => a -> Bool
isPandigital num
    | length x /= 10 = False
    | check          = True
    | otherwise      = False
    where check = (sum x == 45) && (product y == product [1..9])
          x     = num2DigitsR num
          y     = filter (>0) x
          

isSubstringDivisible :: (Integral a) => a -> Bool
isSubstringDivisible n = foldr1 (&&) $ zipWith isDiv substrings primeDivisors
    where substrings = map (`mod` 1000) $ map (n `div`) $ map (10^)[0..6]
          primeDivisors = reverse $ take 7 primes
          isDiv = \x y -> if (x `mod` y) == 0 then True else False
          

substringDivPandigitals :: (Integral a) => [a]
substringDivPandigitals = filter isPandigital $ filter isSubstringDivisible [1023456789,1023456797..9876543210]
