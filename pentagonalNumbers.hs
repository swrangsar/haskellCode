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