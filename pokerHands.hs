import Data.Char (digitToInt)


cardValue :: Char -> Int
cardValue 'T'  = 10
cardValue 'J'  = 11
cardValue 'Q'  = 12
cardValue 'K'  = 13
cardValue 'A'  = 14
cardValue c    = digitToInt c


--handPairs :: String -> [(String, String)]
handPairs s = map split $ map words $ lines s
    where cardTuple (v:s:[])   = (cardValue v, s)
          split xs  = (take 5 ys, drop 5 ys)
              where ys = map cardTuple xs
              
main = do
    s <- readFile "poker.txt"
    putStrLn $ show $ handPairs s
    return ()
    

{--
handValue :: [(Int,Char)] -> Int
handValue h
    where simVals   = (5-) $ length $ nubBy (\a b -> fst a == fst b) h
--}

onePair :: [(Int, Char)] -> Bool
onePair h = 4 == count
    where count = (5-) $ length $ nubBy (\a b -> fst a == fst b) h
    
twoPair :: [(Int, Char)] -> Bool
twoPair h = (3 == count) && (2 == count2)
    where notSimVals    = nubBy (\a b -> fst a == fst b) h
          count         = (5-) $ length notSimVals
          count2        = length $ nubBy (\a b -> fst a == fst b) $ h `minus` notSimVals

threeKind :: [(Int, Char)] -> Bool
threeKind h = (3 == count) && (1 == count2)
    where notSimVals    = nubBy (\a b -> fst a == fst b) h
          count         = (5-) $ length notSimVals
          count2        = length $ nubBy (\a b -> fst a == fst b) $ h `minus` notSimVals


straight :: [(Int, Char)] -> Bool
straight h = all (==1) $ zipWith (-) (tail vals) vals
    where vals = sort $ map fst h

flush :: [(Int, Char)] -> Bool
flush h = all (==x) xs
    where suits     = map snd h
          (x:xs)    = suits

fullHouse :: [(Int, Char)] -> Bool
fullHouse h = (2 == count) && (2 == count2)
    where notSimVals    = nubBy (\a b -> fst a == fst b) h
          count         = (5-) $ length notSimVals
          count2        = length $ nubBy (\a b -> fst a == fst b) $ h `minus` notSimVals

fourKind :: [(Int, Char)] -> Bool
fourKind h = (2 == count) && (1 == count2)
    where notSimVals    = nubBy (\a b -> fst a == fst b) h
          count         = (5-) $ length notSimVals
          count2        = length $ nubBy (\a b -> fst a == fst b) $ h `minus` notSimVals











