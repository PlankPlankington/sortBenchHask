import System.Random
import Data.List
import Criterion.Main


-- Random list generation
genList :: (Show a, Random a) => Int -> a -> a -> IO [a]
genList length t b = do
    seed <- newStdGen 
    return $ take length $ randomRs (t, b) seed

-- Quicksort
quicksort :: [Int] -> [Int]
quicksort list = case list of
    [] -> []
    x:xs -> let 
                bottom = quicksort [a | a <- xs, a <= x]
                top = quicksort [a | a <- xs, a > x]
            in bottom ++ [x] ++ top

-- Bubblesort
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

-- Mergesort
mergesort' ::[Int] -> [Int] -> [Int]
mergesort' lx ly = case (lx, ly) of
    (x, []) -> x
    ([], y) -> y
    (x:xs, y:ys) -> if x < y then x:mergesort' xs (y:ys)
                             else y:mergesort' ys (x:xs)

mergesort :: [Int] -> [Int]
mergesort list = let (half,m,m') = (length list `div` 2, mergesort, mergesort') in
    case (take half list, drop half list) of
        (xs, []) -> xs 
        ([], ys) -> ys 
        ([x], [y]) -> if x < y then x:[y] else y:[x]
        (x:xs, y:ys) -> if x < y then x:m' (m xs) (m (y:ys))
                                 else y:m' (m (x:xs)) (m ys)

-- Benchmarking
bench' :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> IO ()
bench' la lb lc ld le = defaultMain [
    bgroup "Bubble Sort" [ 
        bench "1000 Elements" $ whnf bubblesort la,
        bench "2000 Elements" $ whnf bubblesort lb,
        bench "4000 Elements" $ whnf bubblesort lc,
        bench "8000 Elements" $ whnf bubblesort ld,
        bench "16000 Elements" $ whnf bubblesort le
        ],
    bgroup "Merge Sort" [ 
        bench "1000 Elements" $ whnf mergesort la,
        bench "2000 Elements" $ whnf mergesort lb,
        bench "4000 Elements" $ whnf mergesort lc,
        bench "8000 Elements" $ whnf mergesort ld,
        bench "16000 Elements" $ whnf mergesort le
        ],
    bgroup "Quick Sort" [ 
        bench "1000 Elements" $ whnf quicksort la,
        bench "2000 Elements" $ whnf quicksort lb,
        bench "4000 Elements" $ whnf quicksort lc,
        bench "8000 Elements" $ whnf quicksort ld,
        bench "16000 Elements" $ whnf quicksort le
        ]
    ]

-- Passing generated lists to benchmarking
main = do
    hun <- genList 1000 0 1000 :: IO [Int]
    tho <- genList 2000 0 10000 :: IO [Int]
    tth <- genList 4000 0 100000 :: IO [Int]
    hth <- genList 8000 0 1000000 :: IO [Int]
    mil <- genList 16000 0 10000000 :: IO [Int]
    bench' hun tho tth hth mil
 
