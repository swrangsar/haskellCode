import Data.Char



commaSepValues :: String -> [String]
commaSepValues s = case dropWhile (==',') s of
                        "" -> []
                        s' -> w : commaSepValues s''
                            where (w, s'') = break (==',') s'


removeQuotes :: String -> String
removeQuotes s = filter (/='"') s

char2num :: Char -> Int
char2num c = ord c - ord 'A' + 1

word2num :: String -> Int
word2num = sum.(map char2num)



isTriangle :: (Integral a) => a -> Bool
isTriangle n
    | n <= 0 = False
    | n == 1 = True
    | t == (2*x) = True
    | otherwise = False
    where t = root * (root + 1)
          root = (truncate.sqrt.(*2).fromIntegral) n
          x = (truncate.fromIntegral) n
    
