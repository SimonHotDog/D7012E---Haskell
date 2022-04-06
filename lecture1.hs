import Data.Char (ord,chr)


-- 9 

square :: Int -> Int
square n = n * n

fibs = 0:1:zipWith (+) fibs (tail fibs)

-- Example: zipWith (+) [1,2,3] [4,5,6]

primes :: [Int]
primes = filterPrime [2..]
  where
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

-- take :: Int -> [a] -> [a] returns a prefix of a list


-- 11 

double :: Int -> Int
double n = 2*n

-- 12

dubSq1 :: Int -> Int
dubSq1 n = double (square n)

dubSq2 :: Int -> Int
dubSq2 = double . square


-- 13

xor1 :: Bool -> Bool -> Bool
xor1 x y = x && not y || not x && y

xor2 :: Bool -> Bool -> Bool
xor2 False False = False
xor2 True True = False
xor2 x y = True

nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 x y = True


-- 14 

max2 :: Int -> Int -> Int
max2 x y = 
  if x >= y then x else y


-- 15

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 (max2 a b) c

f :: Int -> Int
f 1 = 0 -- pattern matching
f n = if  n `mod` 2 == 0 then n `div` 2 else 3*n + 1


-- 16

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

isLower :: Char -> Bool
isLower ch = ('a' <= ch) && (ch <= 'z') 

offset :: Int
offset = ord 'A' - ord 'a'

toUpper :: Char -> Char
toUpper ch = chr (ord ch + offset)


-- 17

average :: Float -> Float -> Float
average x1 x2 = (x1 + x2) / 2.0

dist :: Float -> Float -> Float -> Float -> Float
dist x1 y1 x2 y2 = sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

dist' :: Float -> Float -> Float -> Float -> Float
dist' x1 y1 x2 y2 = sqrt(dx*dx + dy*dy)
  where
        dx = x1-x2
        dy = y1-y2


-- 18

-- NB! classes and instances are the topic of future lectures

class Inc a where
  inc :: a -> a

instance Inc Int where
  inc n = n + 1

instance Inc Float where
  inc r = r + 0.5

instance Inc Char where
  inc c = chr ((ord c + 1) `mod` 256)

instance Inc Bool where
  inc False = True
  inc True = True

incTwice :: Inc a => a -> a
incTwice x = inc (inc x)

incTwice' :: Inc a => a -> a
incTwice'  = inc . inc


-- 19

max2' :: Int -> Int -> Int
max2' x y
  | x  >=  y  =  x
  | otherwise  =  y

max3' :: Int -> Int -> Int -> Int
max3' x y z
 | x >= y && x >= z = x
 | y >= z = y
 | otherwise = z

infixl 5 ^^^ 

(^^^) :: Int -> Int -> Int
(^^^) x  y = max2 x y


-- 21

fib :: Int-> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1)  +  fib (n - 2)

fac :: Int-> Int
fac 0 = 1
fac n = n * fac (n - 1)

f' :: Int -> Int
f' n
 | n == 1 = 0
 | n `mod` 2 == 0 = n `div` 2
 | otherwise = 3*n + 1

life_length :: Int -> Int
life_length 1 = 0
life_length x = 1 + life_length(f' x)

