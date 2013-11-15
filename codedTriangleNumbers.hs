<<<<<<< HEAD
commaSepValues :: String -> [String]
commaSepValues s = case dropWhile (==',') s of
                        "" -> []
                        s' -> w : commaSepValues s''
                            where (w, s'') = break (==',') s'
=======
import Data.Char


codedWords :: String -> [String]
codedWords s =  words $ map (\y -> if y == ',' then ' ' else y) $ filter (/='"') s

char2num :: Char -> Int
char2num c = ord c - ord 'A' + 1

word2num :: String -> Int
word2num = sum.(map char2num)



isTriangle :: (Integral a) => a -> Bool
isTriangle n
    | n <= 0 = False
    | t == x = True
    | otherwise = False
    where t = root * (root + 1)
          root = (truncate.sqrt.(*2).fromIntegral) n
          x = (truncate.(*2).fromIntegral) n
    
codedNumbers :: String -> [Int]
codedNumbers s =  map word2num $ codedWords s

requiredCount :: String -> Int
requiredCount s = length $ filter id $ map isTriangle $ codedNumbers s
>>>>>>> 27a37ff74ca89c11b68d54001f8f6f2691fdbe07
