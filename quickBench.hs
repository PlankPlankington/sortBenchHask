import System.Random
import Data.List
import Criterion.Main

genList :: (Show a, Random a) => Int -> a -> a -> IO [a]
genList length t b = do
    seed <- newStdGen 
    return $ take length $ randomRs (t, b) seed

quicksort :: [Int] -> [Int]
quicksort list = case list of
    [] -> []
    x:xs -> let 
                bottom = quicksort [a | a <- xs, a <= x]
                top = quicksort [a | a <- xs, a > x]
            in bottom ++ [x] ++ top

transfer :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> IO ()
transfer la lb lc ld le = defaultMain [
    bgroup "quick" [ 
        bench "100" $ whnf quicksort la,
        bench "1000" $ whnf quicksort lb,
        bench "10000" $ whnf quicksort lc,
        bench "100000" $ whnf quicksort ld,
        bench "1000000" $ whnf quicksort le
        ]
    ]

main = do
    hun <- genList 100 0 1000 :: IO [Int]
    tho <- genList 1000 0 10000 :: IO [Int]
    tth <- genList 10000 0 100000 :: IO [Int]
    hth <- genList 100000 0 1000000 :: IO [Int]
    mil <- genList 1000000 0 10000000 :: IO [Int]
    transfer hun tho tth hth mil
 
