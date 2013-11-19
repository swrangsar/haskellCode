



num2DigitsR :: (Integral a) => a -> [a]
num2DigitsR x
    | quot == 0         = x:[]
    | otherwise         = rem:(num2DigitsR quot)
    where quot          = x `div` 10
          rem           = x `mod` 10


has5PermutedMultiples :: (Integral a) => a -> Bool
has5PermutedMultiples n
    | not (all (==l) ls)        = False
    | not (all (==s) ss)        = False
    | not (all (==nl) nls)      = False
    | not (all (==p) ps)        = False
    | otherwise                 = True
    where   sixMultiples        = map num2DigitsR $ map (n*) [1..6]
            lengths@(l:ls)      = map length sixMultiples
            sums@(s:ss)         = map sum sixMultiples
            noZeroes            = map (filter (>0)) sixMultiples
            lens@(nl:nls)       = map length noZeroes
            prods@(p:ps)        = map product noZeroes
            


candidates = filter has5PermutedMultiples [2..]
            
