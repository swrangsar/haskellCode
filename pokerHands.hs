import Data.Char (digitToInt)
import Data.List


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

              


freq :: Ord a => [a] -> [Int]
freq l = map snd $ map (\x -> (head x, length x)) $ group $ sort l


onePair :: Hand -> Bool
onePair h = any (>1) $ freq $ map fst h
    
twoPair :: Hand -> Bool
twoPair h =  (>1) $ length $ filter (>1) $ freq $ map fst h
    
    
threeKind :: Hand -> Bool
threeKind h = any (>2) $ freq $ map fst h


straight :: Hand -> Bool
straight h = all (==1) $ zipWith (-) (tail vals) vals
    where vals = sort $ map fst h

flush :: Hand -> Bool
flush h = all (==x) xs
    where suits     = map snd h
          (x:xs)    = suits

fullHouse :: Hand -> Bool
fullHouse h = all (>1) $ freq $ map fst h

fourKind :: Hand -> Bool
fourKind h = any (>3) $ freq $ map fst h

stFlush :: Hand -> Bool
stFlush h = (flush h) && (straight h)

royalFlush :: Hand -> Bool
royalFlush h = (flush h) && (all (>9) $ map fst h)

handValue :: Hand -> Int
handValue h
    | royalFlush h      = 10
    | stFlush h         = 9
    | fourKind h        = 8
    | fullHouse h       = 7
    | flush h           = 6
    | straight h        = 5
    | threeKind h       = 4
    | twoPair h         = 3
    | onePair h         = 2
    | otherwise         = 1



fstPlayerWins :: (Hand, Hand) -> Bool
fstPlayerWins (g, h)
    | a > b     = True
    | a == b    = compPrimal g h
    | otherwise = False
    where a = handValue g
          b = handValue h
          uniqFsts l        = map fst $ reverse $ sortBy (\x y -> compare (snd x) (snd y))$ map (\x -> (head x, length x)) $ (group.sort) $ map fst l
          compPrimal x y = (s > t)
              where s = uniqFsts x
                    t = uniqFsts y
                    
                    

main = do
    s <- readFile "poker.txt"
    putStrLn $ show $ length $ filter id $ map fstPlayerWins $ handPairs s
    return ()







