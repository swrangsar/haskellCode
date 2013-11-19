import RabinMiller


num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0         = x:[]
    | otherwise         = rem:(num2DigitsR quot)
    where quot          = x `div` 10
          rem           = x `mod` 10


digitsR2Num :: (Integral a) => [a] -> a
digitsR2Num l = foldr f 0 l
                where f = \y x -> x*10 + y



has8PrimeDigitReplacements :: (Integral a) => a -> Bool
has8PrimeDigitReplacements p
    | min > 2           = False
    | numOfPrimes > 7   = True
    | otherwise         = False
    where   (x:xs)  = num2DigitsR p
            min     = minimum xs
            replaceMinWith r    = x:map (\y -> if y == min then r else y) xs
            newDigitNums        = map replaceMinWith [min..9]
            newNums             = map digitsR2Num newDigitNums
            numOfPrimes         = length $ filter id $ map RabinMiller.isRabinMillerPseudoPrime newNums


candidates = filter has8PrimeDigitReplacements $ filter RabinMiller.isRabinMillerPseudoPrime [56003,56005..]
                       
