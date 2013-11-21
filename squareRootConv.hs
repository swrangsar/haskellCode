import RhoFactors
import Data.List



rhoFacts :: (Integral a) => a -> [a]
rhoFacts = RhoFactors.rhoFactors

innerConv :: (Integral a) => a -> [(a,a)]
innerConv n = irec (2,1) 1
    where irec x r
            | r <= n    = x:irec y (r+1)
            | otherwise = []
            where (a,b)     = x
                  y         = addFrac 2 (b,a)

squareRootConv :: (Integral a) => (a,a) -> (a,a)
squareRootConv (a,b) = addFrac 1 (b,a)


addFrac :: (Integral a) => a -> (a,a) -> (a,a)
addFrac a (c,d) = (m,n)
    where xs    = rhoFacts $ a*d + c
          ys    = rhoFacts $ d
          cs    = intersect xs ys
          m     = product $ 1:(xs \\ cs)
          n     = product $ 1:(ys \\ cs)

num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0     = x:[]
    | otherwise	    = rem:(num2DigitsR quot)
    where quot = x `div` 10
          rem  = x `mod` 10


isBiggerNum :: (Integral a) => (a,a) -> Bool
isBiggerNum (a,b) = l > k
    where l         = length $ num2DigitsR a
          k         = length $ num2DigitsR b
    



reqdCount = length $ filter isBiggerNum $ map squareRootConv $ innerConv 1000