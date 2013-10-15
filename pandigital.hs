firstDigit :: (Integral a) => a -> a
firstDigit x
    | quot == 0 = x
    | otherwise = firstDigit quot
    where quot = x `div` 10

hasFirstDigit9 :: (Integral a) => a -> Bool
hasFirstDigit9 x
    | (firstDigit x) == 9   = True
    | otherwise             = False

num2Digits :: (Integral a) => a -> [a]
num2Digits x
    | quot == 0  = x:[]
    | otherwise = rem:(num2Digits quot)
    where quot = x `div` 10
          rem  = x `mod` 10

listHasUniqueNonZeroDigits :: (Integral a) => [a] -> Bool
listHasUniqueNonZeroDigits [] = True
listHasUniqueNonZeroDigits list@(x:xs)
    | length list > 9   = False
    | elem 0 list       = False
    | otherwise         = (x `notElem` xs) && (listHasUniqueNonZeroDigits xs)

hasUniqueNonZeroDigits :: (Integral a) => a -> Bool
hasUniqueNonZeroDigits n = listHasUniqueNonZeroDigits $ num2Digits n

isPandigital :: (Integral a) => a -> Bool
isPandigital x
    | length digits > 9  = False
    | check         = True
    | otherwise     = False
    where check = (product digits == product [1..9]) && (sum digits == sum [1..9])
          digits = num2Digits x
