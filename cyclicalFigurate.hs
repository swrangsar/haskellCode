triangles :: (Integral a) => [a]
triangles = map triangle [1..]
    where triangle n = (n * (n+1)) `div` 2



squares :: (Integral a) => [a]
squares = map square [1..]
    where square n = n * n

pentagonals :: (Integral a) => [a]
pentagonals = map pentagonal [1..]
    where pentagonal n = (n * (3*n-1)) `div` 2


hexagonals :: (Integral a) => [a]
hexagonals = map hexagonal [1..]
    where hexagonal n = n * (2*n-1)
    
heptagonals :: (Integral a) => [a]
heptagonals = map heptagonal [1..]
    where heptagonal n = (n * (5*n-3)) `div` 2
    

octagonals :: (Integral a) => [a]
octagonals = map octagonal [1..]
    where octagonal n = n * (3*n-2)


fourDigits :: (Integral a) => [a] -> [a]
fourDigits l = takeWhile (< 10000) $ dropWhile (<1000) l


isPair :: (Integral a) => a -> a -> Bool
isPair a b = (inSet a b) || (inSet b a)
    where inSet m n
            | x == y    = True
            | otherwise = False
            where x = mod m 100
                  y = div n 100


candidates = [[a,b,c,d,e,f] | 
    a <- fourDigits triangles,
    b <- (filter (isPair a) $ fourDigits squares),
    c <- (filter (isPair b) $ fourDigits pentagonals),
    d <- (filter (isPair c) $ fourDigits hexagonals),
    e <- (filter (isPair d) $ fourDigits heptagonals),
    f <- (filter (isPair e) $ fourDigits octagonals)]
    
    
    
    
