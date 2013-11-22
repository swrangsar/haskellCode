
num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0     = x:[]
    | otherwise	    = rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10


isBiggerNum :: (Integral a) => (a,a) -> Bool
isBiggerNum (a,b) = l > k
    where l         = length $ num2DigitsR a
          k         = length $ num2DigitsR b
    


expansions :: (Integral a) => a -> [(a,a)]
expansions n = frec (3,2) 1
    where frec (a,b) r
            | r < n         = (a,b):frec (c,d) (r+1)
            | r == n        = (a,b)     
            | otherwise     = []
            where c = 2*b + a
                  d = a+b
                  
                  
                  
                  
reqdCount = length $ filter isBiggerNum $ expansions 1000
