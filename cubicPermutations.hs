import Data.List




num2Digits :: (Integral a) => a -> [a]
num2Digits x
    | quot == 0 = x:[]
    | otherwise        = rem:(num2Digits quot)
    where quot = x `div` 10
          rem  = x `mod` 10



candidates = [l | a <- [1..100000],
    let len = length $ num2Digits a,
    b <- [(a+1)..(10^len-1)],
    c <- [(b+1)..(10^len-1)],
    d <- [(c+1)..(10^len-1)],
    e <- [(d+1)..(10^len-1)],
    let l = [a,b,c,d,e],
    let cs = map (num2Digits.(^3)) l,
    length (nub (map length cs)) == 1,
    (length $ nub $ map length $ map (filter (==0)) cs) == 1,
    length (nub (map sum cs)) == 1,
    (length $ nub $ map product $ map (filter (/=0)) cs) == 1]