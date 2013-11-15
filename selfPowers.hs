base :: (Integral a) => a
base = 10000000000

selfSum :: (Integral a) => a -> a -> a
selfSum a b = mod (a+b) base


selfProduct :: (Integral a) => a -> a -> a
selfProduct a b = mod (a*b) base

selfExpo :: (Integral a) => a -> a -> a
selfExpo a 0 = 1
selfExpo a n = selfProduct a (selfExpo a (n-1))


selfPower :: (Integral a) => a -> a
selfPower n = selfExpo n n

reqdNum = foldl1 selfSum $ map selfPower [1..1000]
