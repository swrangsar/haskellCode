import Data.List


primes :: (Integral a) => [a]
primes = 2:filter isPrime [3,5..]


isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p s -> p*p > n || ((n `mod` p) /= 0 && s))
                True primes


num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0 = x:[]
    | otherwise	= rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10

{--
          listHasUniqueDigits :: (Integral a) => [a] -> Bool
listHasUniqueDigits [] = True
listHasUniqueDigits list@(x:xs) = (x `notElem` xs) && (listHasUniqueDigits xs)

hasUniqueDigits :: (Integral a) => a -> Bool
hasUniqueDigits n = listHasUniqueDigits $ num2DigitsR n
--}

isPermutation :: (Integral a) => a -> a -> Bool
isPermutation a b = (sort $ num2DigitsR a) == (sort $ num2DigitsR b)

zeroCount n = length $ filter (<1) $ num2DigitsR n
onlyOneZero n = (zeroCount n) < 2

-- digitSum n = (sum.num2DigitsR) n
-- digitProduct n = (product.(filter (>0)).num2DigitsR) n

fourDigitPrimes = dropWhile (<1000) $ takeWhile (<9999) primes
goodPrimes = filter onlyOneZero $ fourDigitPrimes
diffPrimes = [(a,b,c) | a <- goodPrimes, b <- goodPrimes, c <- goodPrimes,  a < b, b < c,  (b-a) == (c-b)]

--sumPrimes = [(a,b,c) | (a,b,c) <- diffPrimes, digitSum a == digitSum b, digitSum b == digitSum c]
primePerms = [(a,b,c) | (a,b,c) <- diffPrimes, isPermutation a b, isPermutation b c]


