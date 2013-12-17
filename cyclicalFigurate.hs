import Data.List




<<<<<<< HEAD
triangles :: (Integral a) => [a]
triangles = map triangle [1..]
    where triangle n = (n * (n+1)) `div` 2
=======
fourDigits :: (Integral a) => [a] -> [a]
fourDigits l = takeWhile (< 10000) $ dropWhile (<1000) l


>>>>>>> 0c752183420800533ff72c5545c99f60ca0f71e4

triangles :: (Integral a) => [(a,a)]
triangles = map (\x -> (1,x)) $ fourDigits $ map triangle [1..]
    where triangle n = (n * (n+1)) `div` 2

<<<<<<< HEAD
squares :: (Integral a) => [a]
squares = map square [1..]
    where square n = n * n


pentagonals :: (Integral a) => [a]
pentagonals = map pentagonal [1..]
=======

squares :: (Integral a) => [(a,a)]
squares = map (\x -> (2,x)) $ fourDigits $ map square [1..]
    where square n = n * n

pentagonals :: (Integral a) => [(a,a)]
pentagonals = map (\x -> (3,x)) $ fourDigits $ map pentagonal [1..]
>>>>>>> 0c752183420800533ff72c5545c99f60ca0f71e4
    where pentagonal n = (n * (3*n-1)) `div` 2


hexagonals :: (Integral a) => [(a,a)]
hexagonals = map (\x -> (4,x)) $ fourDigits $ map hexagonal [1..]
    where hexagonal n = n * (2*n-1)
    
<<<<<<< HEAD

heptagonals :: (Integral a) => [a]
heptagonals = map heptagonal [1..]
=======
heptagonals :: (Integral a) => [(a,a)]
heptagonals = map (\x -> (5,x)) $ fourDigits $ map heptagonal [1..]
>>>>>>> 0c752183420800533ff72c5545c99f60ca0f71e4
    where heptagonal n = (n * (5*n-3)) `div` 2

<<<<<<< HEAD
    
octagonals :: (Integral a) => [a]
octagonals = map octagonal [1..]
    where octagonal n = n * (3*n-2)



fourDigits :: (Integral a) => [a] -> [a]
fourDigits l = takeWhile (< 10000) $ dropWhile (<1000) l


isPair :: (Integral a) => a -> a -> Bool
isPair a b = (inSet a b) || (inSet b a)
=======
octagonals :: (Integral a) => [(a,a)]
octagonals = map (\x -> (6,x)) $ fourDigits $ map octagonal [1..]
    where octagonal n = n * (3*n-2)


>>>>>>> 0c752183420800533ff72c5545c99f60ca0f71e4


inSet :: (Integral a) => (a,a) -> (a,a) -> Bool
inSet m n
    | x == y    = True
    | otherwise = False
<<<<<<< HEAD
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
                      
            









=======
    where x = mod (snd m) 100
          y = div (snd n) 100


notTriangles = squares ++ pentagonals ++ hexagonals ++ heptagonals ++ octagonals

candidates = [l | 
    a <- triangles,
    b <- (filter (inSet a) notTriangles),
    c <- (filter (inSet b) notTriangles),
    d <- (filter (inSet c) notTriangles),
    e <- (filter (inSet d) notTriangles),
    f <- (filter (inSet e) notTriangles),
    let l = [a,b,c,d,e,f],
    (length (nub (map fst l))) > 5,
    inSet f a]
    
 answer = sum $ map snd $ head candidates
>>>>>>> 0c752183420800533ff72c5545c99f60ca0f71e4
