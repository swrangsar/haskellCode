{-- ideas from 
    http://qandwhat.apps.runkite.com/i-failed-a-twitter-interview/
    http://philipnilsson.github.io/Badness10k/articles/waterflow/ [accessed oct,2013] 
    the twitter waterflow problem --}

waterFlow :: (Integral a) => [a] -> a
waterFlow [] = 0
waterFlow hs = sum waterHeights
               where leftMaxs       = scanl1 max hs
                     rightMaxs      = scanr1 max hs
                     waterLevels    = zipWith min leftMaxs rightMaxs
                     waterHeights   = zipWith (-) waterLevels hs