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
                  

isTriangle :: (Integral a) => a -> Bool
isTriangle n
    | n <= 0 = False
    | t == x = True
    | otherwise = False
    where t = root * (root + 1)
          root = (truncate.sqrt.(*2).fromIntegral) n
          x = (truncate.(*2).fromIntegral) n


hexagonal :: (Integral a) => a -> a
hexagonal n = n*(2*n - 1)

triPentHex :: (Integral a) => [a]
triPentHex = filter isPentagonal $ filter isTriangle $ map hexagonal [1..]