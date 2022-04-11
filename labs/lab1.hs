-- Simon Lundberg

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Insertion Sort of sublists
ins :: (Int, Int, Int, [Int]) -> [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
ins i [] = [i]
ins (xSize, xf, xl, xList) ((ySize, yf, yl, yList) : ys)
    | xSize <= ySize = xt : yt : ys
    | otherwise = yt : ins xt ys
    where
        xt = (xSize, xf, xl, xList)
        yt = (ySize, yf, yl, yList)

insertionSort :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
insertionSort = foldr ins []

-- Sublist generation

sublistHead :: [Int] -> Int -> [(Int, Int, Int, [Int])]
sublistHead [] _ = []
sublistHead x i = (sum x, i, i + length x - 1, x) : sublistHead (init x) i

sublists :: [Int] -> Int -> [(Int, Int, Int, [Int])]
sublists [] _ = []
sublists x i = sublistHead x i ++ sublists (tail x) (i+1)


-- Final function

smallestK :: [Int] -> Int -> [(Int, Int, Int, [Int])]
smallestK x k = take k (insertionSort (sublists x 1))

--Printing

toString :: [(Int, Int, Int, [Int])] -> String
toString [] = "\n"
toString ((s, i, j, x) : xs) = "\n" ++ show s ++ "\t" ++ show i ++ "\t" ++ show j ++ "\t" ++ show x ++ toString xs 


-- Main program
smallestKSetMain :: [Int] -> Int -> IO ()
smallestKSetMain [] _ = error "List is empty"
smallestKSetMain x k = putStr ("size - start - end - sublist" ++ toString (smallestK x k))

--Test values

l0 :: [Int]
l0 = [-1,2,-3,4,-5]

k0 :: Int 
k0 = 3
-----------

l1 :: [Int]
l1 = [x * (-1) ^x | x <- [1..100]]

k1 :: Int
k1 = 15
-----------

l2 :: [Int]
l2 = [24,-11,-34,42,-24,7,-19,21]

k2 :: Int
k2 = 6
-----------

l3 :: [Int]
l3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3]

k3 :: Int 
k3 = 8
