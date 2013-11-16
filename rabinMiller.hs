reduceEven :: (Integral a) => a -> a -> (a,a)
reduceEven d s
    | even d    = reduceEven d' (s+1)
    | otherwise = (d,s)
    where d' = div d 2
    

modularExpoR :: (Integral a) => a -> a -> a -> a -> a
modularExpoR _ 0 _ r = r
modularExpoR b e m r
    | even e       = modularExpoR b' e' m r
    | (not.even) e = modularExpoR b (e-1) m r'
    where b'    = mod (b*b) m
          e'    = div e 2
          r'    = mod (r*b) m
          
          
modularExpo :: (Integral a) => a -> a -> a -> a
modularExpo _ 0 _ = 1
modularExpo b 1 m = mod b m
modularExpo b e m = modularExpoR b e m 1