import Data.List




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


candidates = map (\x -> map digits2Num x) $ [l | a <- cubes,
    let cubes1 = (tail $ dropWhile (/= a) cubes),
    let alen = length a,
    b <- (takeWhile (\x  -> length x == alen) cubes1),
    let cubes2 = (tail $ dropWhile (/= b) cubes1),
    c <- (takeWhile (\x  -> length x == alen) cubes2),
    let cubes3 = (tail $ dropWhile (/= c) cubes2),
    d <- (takeWhile (\x  -> length x == alen) cubes3),
    let cubes4 = (tail $ dropWhile (/= d) cubes3),
    e <- (takeWhile (\x  -> length x == alen) cubes4),
    let l = [a,b,c,d,e],
    (length $ nub $ map sum l) == 1,
    (length $ nub $ map length $ map (filter (==0)) l) == 1,
    (length $ nub $ map product $ map (filter (/=0)) l) == 1
    ]
