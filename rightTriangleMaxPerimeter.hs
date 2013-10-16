import Data.List


rightTriangles :: (Integral a) => [(a,a,a)]
rightTriangles = [(x,y,z) | x <- [1..500], y <- [x..500], z <- [(y+1)..500], x^2 + y^2 == z^2, x+y+z <= 1000]

perimeters :: (Integral a) => [a]
perimeters = map f rightTriangles
             where f = \(a,b,c) -> a+b+c

perimeterCounts :: (Integral a) => [(a, Int)]
perimeterCounts = map (\x -> (head x, length x)) $ group $ sort perimeters

comparing :: (Integral a) => (a,Int) -> (a, Int) -> Ordering
comparing x y
    | snd x == snd y = compare (fst x) (fst y)
    | otherwise = compare (snd x) (snd y)

maxPerimeterCount :: (Integral a) => (a, Int)
maxPerimeterCount = maximumBy comparing perimeterCounts
