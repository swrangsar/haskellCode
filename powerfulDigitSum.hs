num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0     = x:[]
    | otherwise	    = rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10


digitsR2Num :: (Integral a) => [a] -> a
digitsR2Num l = foldr f 0 l
               where f = \y x -> x*10 + y



maxDigitSum :: (Integral a) => a -> a
maxDigitSum n = maximum $ map digitSum $ map (n^) [1..99]
    where digitSum x = sum $ num2DigitsR x
    



reqdCount = maximum $ map maxDigitSum [1..99]