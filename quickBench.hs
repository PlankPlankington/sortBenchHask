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

bubbleHelp :: [Int] -> [Int]
bubbleHelp list = case list of
    x:y:xs -> if y > x then x:bubbleHelp (y:xs) else y:bubbleHelp (x:xs)
    [x] -> [x]
    [] -> []

bubble :: Int -> [Int] -> [Int]
bubble l xs = case l of
    0 -> xs
    n -> bubble (n-1) (bubbleHelp xs)

bubblesort :: [Int] -> [Int]
bubblesort list = bubble (length list) list

transfer :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> IO ()
transfer la lb lc ld le = defaultMain [
    bgroup "Bubble Sort" [ 
        bench "100 Elements" $ whnf bubblesort la,
        bench "1000 Elements" $ whnf bubblesort lb,
        bench "10000 Elements" $ whnf bubblesort lc,
        bench "100000 Elements" $ whnf bubblesort ld,
        bench "1000000 Elements" $ whnf bubblesort le
        ],
    bgroup "Quick Sort" [ 
        bench "100 Elements" $ whnf quicksort la,
        bench "1000 Elements" $ whnf quicksort lb,
        bench "10000 Elements" $ whnf quicksort lc,
        bench "100000 Elements" $ whnf quicksort ld,
        bench "1000000 Elements" $ whnf quicksort le
        ]
    ]

main = do
    hun <- genList 100 0 1000 :: IO [Int]
    tho <- genList 1000 0 10000 :: IO [Int]
    tth <- genList 10000 0 100000 :: IO [Int]
    hth <- genList 100000 0 1000000 :: IO [Int]
    mil <- genList 1000000 0 10000000 :: IO [Int]
    transfer hun tho tth hth mil
 
