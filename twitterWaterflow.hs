{-- ideas from http://philipnilsson.github.io/Badness10k/articles/waterflow/ [accessed oct,2013] --}

waterFlow :: (Integral a) => [a] -> a
waterFlow [] = 0
waterFlow hs = sum waterHeights
               where leftMaxs       = scanl1 max hs
                     rightMaxs      = scanr1 max hs
                     waterLevels    = zipWith min leftMaxs rightMaxs
                     waterHeights   = zipWith (-) waterLevels hs