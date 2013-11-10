-- using Newton's method and using the nearest greater power of 2 --

numbits :: (Integral a) => a -> a
numbits n
    | n < 2     = 1
    | otherwise = 1 + (numbits (n `div` 2))


newtonsMethod :: (RealFrac a) => a -> a -> a
newtonsMethod x n
    | y < x         = newtonsMethod y n
    | otherwise     = x
    where y         = (x + (n/x))/2
    
    
squareRoot :: (RealFrac a) => a -> a
squareRoot n = newtonsMethod y n
                where x = truncate n
                      y = fromIntegral . (2^) . ceiling . (/2) . toRational $ numbits(x)       
