import Data.List

{-- this is not the most efficient method ! --}


num2Digits :: (Integral a) => a -> [a]
num2Digits x
    | quot == 0 = x:[]
    | otherwise        = rem:(num2Digits quot)
    where quot = x `div` 10
          rem  = x `mod` 10


digits2Num :: (Integral a) => [a] -> a
digits2Num l = foldr f 0 l
               where f = \y x -> x*10 + y



cubes :: (Integral a) => [[a]]
cubes = map num2Digits $ map (^3) [1..]


candidates =  map (\x -> map digits2Num x) [l | a <- cubes,
    let cubes1 = (tail $ dropWhile (/= a) cubes),
    let alen = length a,
    b <- (takeWhile (\x  -> length x == alen) cubes1),
    let a' = sort a,
    let b' = sort b,
    a' == b',
    let cubes2 = (tail $ dropWhile (/= b) cubes1),
    c <- (takeWhile (\x  -> length x == alen) cubes2),
    let c' = sort c,
    c' == a',
    let cubes3 = (tail $ dropWhile (/= c) cubes2),
    d <- (takeWhile (\x  -> length x == alen) cubes3),
    let d' = sort d,
    d' == a',
    let cubes4 = (tail $ dropWhile (/= d) cubes3),
    e <- (takeWhile (\x  -> length x == alen) cubes4),
    let e' = sort e,
    e' == a',
    let l = [a,b,c,d,e]]
