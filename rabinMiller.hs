reduceEven :: (Integral a) => a -> a -> (a,a)
reduceEven d s
    | even d    = reduceEven d' (s+1)
    | otherwise = (d,s)
    where d' = div d 2
    


