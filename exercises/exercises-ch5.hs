
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}

-- 5.2

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a,b,c)
    | a <= b && b <= c = (a,b,c)
    | a <= c && c <= b = (a,c,b)
    | b <= a && a <= c = (b,a,c)
    | b <= c && c <= a = (b,c,a)
    | c <= a && a <= b = (c,a,b)
    | otherwise = (c,b,a)


-- 5.10

divisors :: Int -> [Int]
divisors n = [i | i<-[1 .. n], mod n i == 0]

isPrime :: Int -> Bool
isPrime n
    | n >= 0 = length (divisors n) == 2
    | otherwise = False


-- 5.11

matches :: Int -> [Int] -> [Int]
matches n list = [i | i<-list, i == n]

elem :: Int -> [Int] -> Bool
elem n list = length (matches n list) > 0


-- 5.18

shift :: ((Int,Int),Int) -> (Int, (Int,Int))
shift ((x,y),z) = (x,(y,z))


-- 5.22

onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines noLines
    | length noLines == 1 = head noLines
    | otherwise = head noLines ++ "\n" ++ onSeparateLines (tail noLines)


-- 5.23

duplicate :: String -> Int -> String
duplicate str n
    | n <= 0 = ""
    | n == 1 = str
    | otherwise = str ++ duplicate str (n-1)


-- 5.24  Could apparently be useful in lab 1...

pushRight :: Int -> String -> String
pushRight ll str
    | ll == 0 = str
    | otherwise = " " ++ pushRight (ll-1) str
