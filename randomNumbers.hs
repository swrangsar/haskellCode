import System.Random

main = do
    g <- newStdGen
    print $ take 10 (randomRs (1,9) g :: [Int])