import RabinMiller


isPrime :: (Integral a) => a -> Bool
isPrime = RabinMiller.isRabinMillerPseudoPrime


spiralPrimes :: (Integral a) => a
spiralPrimes = spirec 3 0
    where spirec n r
            | ratio < 0.1   = n
            | otherwise     = spirec (n+2) r'
            where sq    = n*n
                  diff  = n-1
                  a     = sq-diff
                  b     = a-diff
                  c     = b-diff
                  l     = length $ filter isPrime $ [a,b,c]
                  r'    = r + l
                  diags = fromIntegral $ (div (n-1) 2)*4 + 1
                  ratio = (fromIntegral r') / diags