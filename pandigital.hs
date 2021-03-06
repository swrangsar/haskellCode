import Data.List


firstDigit :: (Integral a) => a -> a
firstDigit x
    | quot == 0 = x
    | otherwise = firstDigit quot
    where quot = x `div` 10

hasFirstDigit9 :: (Integral a) => a -> Bool
hasFirstDigit9 x
    | (firstDigit x) == 9   = True
    | otherwise             = False


num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0 = x:[]
    | otherwise	= rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10

num2Digits :: (Integral a) => a -> [a]
num2Digits = reverse.num2DigitsR


listHasUniqueNonZeroDigits :: (Integral a) => [a] -> Bool
listHasUniqueNonZeroDigits [] = True
listHasUniqueNonZeroDigits list@(x:xs)
    | length list > 9   = False
    | elem 0 list       = False
    | otherwise         = (x `notElem` xs) && (listHasUniqueNonZeroDigits xs)

hasUniqueNonZeroDigits :: (Integral a) => a -> Bool
hasUniqueNonZeroDigits n = listHasUniqueNonZeroDigits $ num2Digits n


isPandigital :: (Integral a) => a -> Bool
isPandigital num
    | length x > 9  = False
    | check         = True
    | otherwise     = False
    where check = (product x == product [1..9]) && (sum x == sum [1..9])
          x     = num2DigitsR num

goodCandidates :: (Integral a) => [a]
goodCandidates = filter hasUniqueNonZeroDigits $ filter hasFirstDigit9 [1..10000]

digits2Num :: (Integral a) => [a] -> a
digits2Num l = foldl' f 0 l
               where f = \x y -> x*10 + y

digitsR2Num :: (Integral a) => [a] -> a
digitsR2Num l = foldr f 0 l
               where f = \y x -> x*10 + y

tryFormingPandigital :: (Integral a) => a -> a -> [a] -> [a]
tryFormingPandigital x n acc
    | length acc' < 9   = tryFormingPandigital x (n+1) acc'
    | otherwise         = acc'
    where acc' = num2DigitsR (x*n) ++ acc

trialPandigital :: (Integral a) => a -> a
trialPandigital x = digitsR2Num $ tryFormingPandigital x 1 []

-- main part

reqdPandigitals :: (Integral a) => [a]
reqdPandigitals = filter isPandigital $ map trialPandigital goodCandidates


finalPandigital :: (Integral a) => a
finalPandigital = maximum reqdPandigitals
