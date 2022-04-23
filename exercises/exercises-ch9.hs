
-- 9.2
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}

length :: [t] -> Int
length list = sum (map (\n -> 1) list)

-- 9.6

asSquares :: [Int] -> [Int]
asSquares = map (^ 2)

sumSquares :: [Int] -> Int
sumSquares ns = sum (asSquares ns)

greaterThanZero :: [Int] -> Bool
greaterThanZero = all (> 0)

-- 9.7

minf :: (Int -> Int) -> Int -> Int 
minf f 0 = f 0
minf f n = min (f n) (minf f (n-1))

--valeqf :: (Int -> Int) -> Int -> Bool

