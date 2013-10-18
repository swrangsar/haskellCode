primes = 2 : filter isPrime [3,5..]


isPrime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes


digits :: (Integral a) => a -> [a]
digits x
    | quot == 0 = x:[]
    | otherwise	= rem:(digits quot)
    where quot = x `div` 10
          rem  = x `mod` 10


hasUniqueNonZeroDigits :: (Integral a) => [a] -> Bool
hasUniqueNonZeroDigits [] = True
hasUniqueNonZeroDigits list@(x:xs)
    | length list > 9   = False
    | elem 0 list       = False
    | otherwise         = (x `notElem` xs) && (hasUniqueNonZeroDigits xs)

isPandigital :: (Integral a) => a -> Bool
isPandigital n = hasUniqueNonZeroDigits $ digits n


largestPrimePandigital = maximum $ filter isPandigital $ filter (< 987654321) primes