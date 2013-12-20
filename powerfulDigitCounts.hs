
digitCount :: (Integral a) => a -> a
digitCount n = fromIntegral $ length $ dropWhile (< 10^(n-1)) $ map (^n) [1..9]

result = sum $ takeWhile (>0) $ map digitCount [1..]