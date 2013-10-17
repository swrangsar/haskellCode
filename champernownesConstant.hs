-- import Data.List


-- digits in reverse order !
digits :: (Integral a) => a -> [a]
digits x
    | quot == 0 = x:[]
    | otherwise	= rem:(digits quot)
    where quot = x `div` 10
          rem  = x `mod` 10




digitNth :: (Integral a) => a -> Int -> a
digitNth x n
    | len >= n    = (digits x) !! (len - n)
    | otherwise   = digitNth (x+1) (n-len)
    where len = length $ digits x

digitNthOfIrrational :: (Integral a) => Int -> a
digitNthOfIrrational = digitNth 1

champernownesConstant :: (Integral a) => a
champernownesConstant = product $ map digitNthOfIrrational $ map (10 ^) [0..6]
