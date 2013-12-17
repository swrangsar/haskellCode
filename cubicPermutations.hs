import Data.List




num2Digits :: (Integral a) => a -> [a]
num2Digits x
    | quot == 0 = x:[]
    | otherwise        = rem:(num2Digits quot)
    where quot = x `div` 10
          rem  = x `mod` 10

diglen :: (Integral a) => a -> Int
diglen x = length $ num2Digits x

cubes :: (Integral a) => [a]
cubes = map (^3) [1..]


candidates = [l | a <- cubes,
    let cubes1 = dropWhile (<= a) cubes,
    b <- (takeWhile (\x  -> (diglen x) == (diglen a)) cubes1),
    let cubes2 = dropWhile (<= b) cubes1,
    c <- (takeWhile (\x  -> (diglen x) == (diglen a)) cubes2),
    let cubes3 = dropWhile (<= c) cubes2,
    d <- (takeWhile (\x  -> (diglen x) == (diglen a)) cubes3),
    let cubes4 = dropWhile (<= d) cubes3,
    e <- (takeWhile (\x  -> (diglen x) == (diglen a)) cubes4),
    let l = [a,b,c,d,e],
    let cs = map num2Digits l,
    (length $ nub $ map sum cs) == 1,
    (length $ nub $ map length $ map (filter (==0)) cs) == 1,
    (length $ nub $ map product $ map (filter (/=0)) cs) == 1
    ]