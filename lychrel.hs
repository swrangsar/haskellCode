

num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0     = x:[]
    | otherwise	    = rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10


digitsR2Num :: (Integral a) => [a] -> a
digitsR2Num l = foldr f 0 l
               where f = \y x -> x*10 + y


isLychrel :: (Integral a) => a -> Bool
isLychrel n = lrec n 1
    where lrec x r
              | r >= 50             = True
              | (ys == reverse ys)  = False
              | otherwise           = lrec y (r+1)
              where xs  = num2DigitsR x
                    y   = (digitsR2Num xs) + (digitsR2Num $ reverse xs)
                    ys  = num2DigitsR y


reqdCount = length $ filter isLychrel [1..10000]