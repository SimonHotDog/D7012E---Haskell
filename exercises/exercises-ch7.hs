{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use or" #-}
import Prelude hiding (splitAt, drop, unzip, reverse, or, and)


-- 7.2

pattern :: [Int] -> Int
pattern [] = 0
pattern [x] = x
pattern(x:y:xs) = x+y


-- 7.3 - Only the second half

noPattern :: [Int]-> Int
noPattern list
    | length list >= 2 = head list + head (tail list)
    | length list == 1 = head list
    | otherwise = 0


-- 7.4 

productNoPattern :: [Int] -> Int
productNoPattern lst
    | null lst = 1
    | otherwise = head lst * productNoPattern (tail lst)

-- A pattern matching version, since I just remembered that it was the topic of the chapter... 
productPattern :: [Int] -> Int
productPattern = foldr (*) 1

-- 7.5

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False


-- 7.7

numel :: Int -> [Int] -> Int -- This function also doubles as a solution to 7.6
numel y [] = 0
numel y (x:xs)
    | y == x = 1 + numel y xs
    | otherwise = numel y xs


unique :: [Int] -> [Int] -- List Comprehension solution
unique lst = [x | x <- lst, numel x lst == 1]

rmElement :: Int -> [Int] -> [Int] -- Removes all instances of element from list.
rmElement y [] = []
rmElement y (x:xs)
    | x /= y = x:rmElement y xs
    | otherwise = rmElement y xs



unique2 :: [Int] -> [Int]
unique2 [] = []
unique2 (x:xs)
    | numel x xs == 0 = x:unique2 xs
    | otherwise = unique2 (rmElement x xs)


-- 7.8

reverse :: [t] -> [t]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((x,y):ps) = (x:xs,y:ys)
    where (xs, ys) = unzip ps


-- 7.9

iSort :: [Int] -> [Int] -- Performs insertion sort
iSort = foldr ins []

ins :: Int -> [Int] -> [Int] -- Inserts integer into a list of integers at the proper ordered position
ins x [] = [x]
ins x (y:ys)
    | x <= y = x:(y:ys)
    | otherwise = y:ins x ys


-- 7.14

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n (x:xs)
    | n > 0 = drop (n-1) xs
    | n == 0 = x:xs
    | n < 0 = error "PreludeList.drop: negative argument"

splitAt :: Int -> [a] -> ([a],[a])
splitAt _ [] = ([],[])
splitAt 0 lst = ([], lst)
splitAt n (x:xs) = (x:y,z)
    where (y,z) = splitAt (n-1) xs

