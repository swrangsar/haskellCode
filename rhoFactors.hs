import Data.List (sort)
import RabinMiller


isPrime :: (Integral a) => a -> Bool
isPrime = RabinMiller.isRabinMillerPseudoPrime

rhoFactor :: (Integral a) => a -> a -> a
rhoFactor n c = fact 2 2
    where f x = mod (x*x+c) n
          fact t h
              | d == 1      = fact t' h'
              | d == n      = rhoFactor n (c+1)
              | isPrime d   = d   
              | otherwise   = rhoFactor d (c+1)
                where   t'  = f t
                        h'  = f $ f h
                        d   = gcd (t'-h') n

rhoFactors :: (Integral a) => a -> [a]
rhoFactors n = sort $ facts n
    where facts n
            | n == 2    = [2]
            | even n    = 2:facts (div n 2)
            | isPrime n = [n]
            | otherwise = f:facts (div n f)
            where f = rhoFactor n 1