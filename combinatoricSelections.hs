isCombMil :: (Integral a) => a -> a -> Bool
isCombMil _ 0 = False
isCombMil _ 1 = False
isCombMil n r
    | n == r            = False
    | newNums == []     = False
    | otherwise         = isProdMil newNums 1
    where   nums    = [(max r (n-r))..n]
            dens    = [2..(min r (n-r))]
            reduceFactors list@(x:xs) y
                | list == []        = []
                | (mod x y) == 0    = (div x y):xs
                | otherwise         = x:(reduceFactors xs y)

            reducedNums ns ds
                | ds == []      = ns
                | otherwise     = reducedNums (reduceFactors ns l) ls
                where   (l:ls)  = ds
            newNums             = reducedNums nums dens
            isProdMil (x:xs) r
                | r > 1000000   = True
                | xs == []      = False
                | otherwise     = isProdMil xs (x*r)
        


               
