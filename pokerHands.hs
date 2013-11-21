import Data.Char (digitToInt)


cardValue :: Char -> Int
cardValue 'T'  = 10
cardValue 'J'  = 11
cardValue 'Q'  = 12
cardValue 'K'  = 13
cardValue 'A'  = 14
cardValue c    = digitToInt c


type Hand = [(Int, Char)]

handPairs :: String -> [(Hand, Hand)]
handPairs s = map split $ map words $ lines s
    where cardTuple (v:s:[])   = (cardValue v, s)
          split xs  = (take 5 ys, drop 5 ys)
              where ys = map cardTuple xs
              
main = do
    s <- readFile "poker.txt"
    putStrLn $ show $ handPairs s
    return ()
    

{--
handValue :: Hand -> Int
handValue h
    where simVals   = (5-) $ length $ nubBy (\a b -> fst a == fst b) h
--}

nubFst :: [a] -> [a]
nubFst = nubBy (\a b -> fst a == fst b)


onePair :: Hand -> Bool
onePair h = 4 == count
    where count = (5-) $ length $ nubFst h
    
twoPair :: Hand -> Bool
twoPair h = (3 == count) && (2 == count2)
    where notSimVals    = nubFst h
          count         = (5-) $ length notSimVals
          count2        = length $ nubFst $ h `minus` notSimVals

threeKind :: Hand -> Bool
threeKind h = (3 == count) && (1 == count2)
    where notSimVals    = nubFst h
          count         = (5-) $ length notSimVals
          count2        = length $ nubFst $ h `minus` notSimVals


straight :: Hand -> Bool
straight h = all (==1) $ zipWith (-) (tail vals) vals
    where vals = sort $ map fst h

flush :: Hand -> Bool
flush h = all (==x) xs
    where suits     = map snd h
          (x:xs)    = suits

fullHouse :: Hand -> Bool
fullHouse h = (2 == count) && (2 == count2)
    where notSimVals    = nubFst h
          count         = (5-) $ length notSimVals
          count2        = length $ nubFst $ h `minus` notSimVals

fourKind :: Hand -> Bool
fourKind h = (2 == count) && (1 == count2)
    where notSimVals    = nubFst h
          count         = (5-) $ length notSimVals
          count2        = length $ nubFst $ h `minus` notSimVals


stFlush :: Hand -> Bool
stFlush h = (flush h) && (straight h)

royalFlush :: Hand -> Bool
royalFlush h = (flush h) && (all (>9) $ map fst h)








