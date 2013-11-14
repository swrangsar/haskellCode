isPentagonal :: (Integral a) => a -> Bool
isPentagonal p
    | p <= 0                = False
    | disc /= (root * root) = False
    | (mod nume 6) /= 0     = False
    | otherwise             = True
    where disc  = 1 + (24*p)
          root  = (truncate.sqrt.fromIntegral) disc
          nume  = root + 1

pentagonal :: (Integral a) => a -> a
pentagonal n    = div nume 2
                  where nume = n*(3*n - 1)
                  

candidates = [[a,b,d] | y <- [1..], x <- [1..(y-1)], x > 0, let a = pentagonal x, let b = pentagonal y, isPentagonal (a+b), let d = b - a, isPentagonal d]
requiredCandidate = (!! 2) $ candidates !! 0

isDiffPenta :: (Integral a) => a -> Bool
isDiffPenta diff = smallGeneratorExist diff 1


{-- equation 3ns = d - pentagonal(s) 
    n is the pentagonal generator of the smaller number in the pair --}
smallGeneratorExist :: (Integral a) => a -> a -> Bool
smallGeneratorExist diff s
    | nume < (3*s)              = False
    | (mod nume (3*s)) == 0     = True
    | otherwise                 = smallGeneratorExist diff (s+1)    
    where nume  = diff - (pentagonal s)
    

minimumPentaDiff :: (Integral a) => [a]
minimumPentaDiff = filter isDiffPenta $map pentagonal [1..]

revisedCandidates =[[a,b,d] | y <- [1..], x <- [1..(y-1)], x > 0, let a = pentagonal x, let b = pentagonal y, let d = b - a, isPentagonal d, isDiffPenta d, isPentagonal (a+b)]
