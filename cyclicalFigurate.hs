import Data.List




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


inSet :: (Integral a) => a -> a -> Bool
inSet m n
    | x == y    = True
    | otherwise = False
    where x = mod m 100
          y = div n 100


candidates = [l| 
    a <- fourDigits triangles,
    b <- fourDigits squares,
    c <- fourDigits pentagonals,
    d <- fourDigits hexagonals,
    e <- fourDigits heptagonals,
    f <- fourDigits octagonals,
    let l = [a,b,c,d,e,f],
    isCyclic l]
    
    
isCyclic :: (Integral a) => [a] -> Bool
isCyclic l = rec l
    where   rec s
                | xs == []      = inSet x (head l)
                | ys == []      = False
                | otherwise     = rec (y:xs')
                where (x:xs)    = s
                      ys        = filter (inSet x) xs
                      y         = head ys
                      xs'       = delete y xs
                      
            









