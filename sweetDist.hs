-- the input list: the first value is the cost of each sweet
-- the second value is the number of students, the remaining
-- values are the standards in which the students are studying

students = drop 2 input

distLeft = scanl f (1,students !! 0) students
    where f (a,b) y
            | b < y     = (a+1,y)
            | b == y    = (a,y)
            | otherwise = (1,y)

distRight = scanr g (1, students !! (n-1)) students
    where n = input !! 1
          g = flip f

dist = zipWith max distLeft distRight
minCost = (sum dist) * (input !! 0)