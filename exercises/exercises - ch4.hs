-- Utils

myMax :: Int -> Int -> Int
myMax x y
  | x >= y = x
  | otherwise = y

-- 4.7

mult :: Int -> Int -> Int
mult 0 y = 0
mult x y = mult (x - 1) y + y

-- 4.8

seekRoot :: Int -> Int -> Int
seekRoot n s
  | s^2 > n = s - 1
  | otherwise = seekRoot n (s + 1)

intSqrt :: Int -> Int
intSqrt n = seekRoot n 1

-- 4.9

--Definitions for testing
f :: Int -> Int
f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

recmax :: Int -> Int
recmax 0 = f 0
recmax n = myMax (f n) (recmax (n-1))

-- 4.14
recpower :: Int -> Int
recpower 0 = 1
recpower 1 = 2
recpower n = 2 * recpower (n-1)