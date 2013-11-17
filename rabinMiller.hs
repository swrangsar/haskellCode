-- the naive prime section --

primes :: (Integral a) => [a]
primes = 2:filter isPrime [3,5..]


isPrime :: (Integral a) => a -> Bool
isPrime n = n > 1 &&
              foldr (\p s -> p*p > n || ((n `mod` p) /= 0 && s))
                True primes




{-- the rabin Miller prime section --}

modularExpo :: (Integral a) => a -> a -> a -> a
modularExpo b e m =
    let times x y m = mod (x*y) m
        powrec b e r
            | e == 0    = r
            | even e    = powrec (times b b) (div e 2) r
            | otherwise = powrec (times b b) (div e 2) (times b r)
    in  powrec b e 1



reduceEven :: (Integral a) => a -> a -> (a,a)
reduceEven d s
    | even d    = reduceEven d' (s+1)
    | otherwise = (d,s)
    where d' = div d 2
    


isPseudoPrimeForBase :: (Integral a) => a -> a -> Bool
isPseudoPrimeForBase n a = pseudoPrimeTest t s n
                           where (d,s) = reduceEven (n-1) 0
                                 t = modularExpo a d n


pseudoPrimeTest :: (Integral a) => a -> a -> a -> Bool
pseudoPrimeTest t s n
    | (t==1) || (t==(n-1))  = True
    | s == 0                = False
    | otherwise             = pseudoPrimeTest t' s' n
    where t' = mod (t*t) n
          s' = s-1
          

isRabinMillerPseudoPrime :: (Integral a) => a -> Bool
isRabinMillerPseudoPrime 1 = False
isRabinMillerPseudoPrime n =
    let ps = takeWhile (<100) primes
    in n `elem` ps || all (isPseudoPrimeForBase n) ps




