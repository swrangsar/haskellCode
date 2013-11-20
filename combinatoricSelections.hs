isCombMil :: (Integral a) => a -> a -> Bool
isCombMil _ 0 = False
isCombMil _ 1 = False
isCombMil n r
    | (n-r) < 2         = False
    | comb > 1000000    = True
    | otherwise         = False
    where   r'      = min r (n-r)
            nums    = [(n-r'+1)..n]
            dens    = [1..r']
            comb    = ceiling $ product nums / product dens       


numOfCombMils :: (Integral a) => a -> a
numOfCombMils n = fromIntegral.length $ filter (isCombMil n) [0..n]


reqdCount = sum $ map numOfCombMils [1..100]