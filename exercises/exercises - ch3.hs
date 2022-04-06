-- 3.7
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p
  | m == n || m == p || n == p = False
  | otherwise = True

-- 3.8

twoEqual :: Int -> Int -> Bool
twoEqual a b
  | a == b = True
  | otherwise = False

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = twoEqual a b && twoEqual b c

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = twoEqual d a && threeEqual a b c

-- 3.15
numberNDroots :: Float -> Float -> Float -> Int
numberNDroots a b c
  | b^2 > (4.0 * a * c) = 2
  | b^2 == (4.0 * a * c) = 1
  | otherwise = 0


-- 3.16
numberDroots :: Float -> Float -> Int
numberDroots b c
  | b /= 0.0 = 1
  | b == 0.0 && c /= 0.0 = 0
  | otherwise = 3

numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
  | a /= 0 = numberNDroots a b c
  | otherwise = numberDroots b c

-- 3.17

smallRoot, largeRoot :: Float -> Float -> Float -> Float
smallRoot a b c = ((-b) - sqrt (b^2 - (4.0*a*c))) / (2*a)
largeRoot a b c = ((-b) + sqrt (b^2 - (4.0*a*c))) / (2*a)

root a b c func
  | numberRoots a b c == 3 = 0
  | numberRoots a b c >= 1 = func a b c
  | otherwise = 0

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = root a b c smallRoot

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = root a b c largeRoot