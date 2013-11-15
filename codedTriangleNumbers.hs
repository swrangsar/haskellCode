commaSepValues :: String -> [String]
commaSepValues s = case dropWhile (==',') s of
                        "" -> []
                        s' -> w : commaSepValues s''
                            where (w, s'') = break (==',') s'
