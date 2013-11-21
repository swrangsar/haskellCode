import RhoFactors
import Data.List.Ordered



rhoFactors :: (Integral a) => a -> [a]
rhoFactors = RhoFactors.rhoFactors

squareRootConv :: (Integral a) => a -> (a,a)
squareRootConv n = frec (2,1) n
    where frec x r
            | r > 1     = frec y (r-1)
            | otherwise = z
            where (a,b)     = x
                  y         = addFrac (2,1) (b,a)
                  z         = addFrac (1,1) (b,a)


addFrac :: (Integral a) => (a,a) -> (a,a) -> (a,a)
addFrac (a,b) (c,d) = (m,n)
    where xs    = rhoFactors $ a*d + b*c
          ys    = rhoFactors $ b*d
          cs    = isect xs ys
          m     = product $ 1:(minus xs cs)
          n     = product $ 1:(minus ys cs)

num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0     = x:[]
    | otherwise	    = rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10


isBiggerNum :: (Integral a) => a -> Bool
isBiggerNum n = l > k
    where (a,b)     = squareRootConv n
          l         = length $ num2DigitsR a
          k         = length $ num2DigitsR b
    
    

reqdCount = length $ filter isBiggerNum [1..1000]