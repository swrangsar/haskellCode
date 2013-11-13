
{-- got the prime list --}

num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0 = x:[]
    | otherwise	= rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10



listHasUniqueDigits :: (Integral a) => [a] -> Bool
listHasUniqueDigits [] = True
listHasUniqueDigits list@(x:xs) = (x `notElem` xs) && (listHasUniqueDigits xs)

hasUniqueDigits :: (Integral a) => a -> Bool
hasUniqueDigits n = listHasUniqueDigits $ num2DigitsR n

          
          
          
{-- new thought nov 13 --}

threeDigitDivisibles :: (Integral a) => a -> [a]
threeDigitDivisibles n = filter hasUniqueThree $ filter (>10) $ takeWhile (<1000) $ map (n*) [1..500]

hasUniqueThree :: (Integral a) => a -> Bool
hasUniqueThree n
    | a == b    = False
    | b == c    = False
    | c == a    = False
    | otherwise = True
    where [a,b,c] = map (`mod` 10) $ zipWith div (take 3 $ repeat n) (map (10^) [0..2])
    
    
canBeCombined :: (Integral a) => a -> a -> Bool
canBeCombined a b = (mod a 100) == (div b 10)

fourDigits = filter hasUniqueDigits $ [((div a 100)* 1000 + b) | a <- threeDigitDivisibles 13, b <- threeDigitDivisibles 17, canBeCombined a b]
fiveDigits = filter hasUniqueDigits $ [((div a 100)* 10000 + b) | a <- threeDigitDivisibles 11, b <- fourDigits, (mod a 100) == (div b 100)]
sixDigits = filter hasUniqueDigits $ [((div a 100)* 100000 + b) | a <- threeDigitDivisibles 7, b <- fiveDigits, (mod a 100) == (div b 1000)]
sevenDigits = filter hasUniqueDigits $ [((div a 100)* 1000000 + b) | a <- threeDigitDivisibles 5, b <- sixDigits, (mod a 100) == (div b 10000)]
eightDigits = filter hasUniqueDigits $ [((div a 100)* 10000000 + b) | a <- threeDigitDivisibles 3, b <- sevenDigits, (mod a 100) == (div b 100000)]
nineDigits = filter hasUniqueDigits $ [((div a 100)* 100000000 + b) | a <- threeDigitDivisibles 2, b <- eightDigits, (mod a 100) == (div b 1000000)]
tenDigits = filter hasUniqueDigits $ [(a*1000000000 + b) | a <- [1..9], b <- nineDigits]

finalResult = sum tenDigits






