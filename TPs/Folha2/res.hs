import Data.Char
sum100Squares :: Integer
sum100Squares = sum [i^2 | i <- [1..100]]

aprox :: Int -> Double
aprox n = 4 * sum [((-1)**fromIntegral i) / (2*fromIntegral i+1) | i <- [0..n]]

aprox' :: Int -> Double
aprox' n = sqrt (12 * sum [((-1)**fromIntegral i) / (fromIntegral i+1)**2 | i <- [0..n]])

dotprod :: [Float] -> [Float] -> Float
dotprod x y = sum (zipWith (*) x y)

divprop :: Integer -> [Integer]
divprop n = [i | i <- [1..n-1], mod n i == 0]

perfects :: Integer -> [Integer]
perfects n = [i | i <- [1..n], sum (divprop i) == i]

pithagoreans :: Integer -> [(Integer,Integer,Integer)]
pithagoreans n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], c^2 == a^2 + b^2]

prime :: Integer -> Bool
prime n = divprop n == [1]

fact :: Integer -> Integer
fact n = product [1..n]
binom :: Integer -> Integer -> Integer
binom n k = div (fact n) ( fact k * fact (n-k) )
pascal :: Integer -> [[Integer]]
pascal n = [map (binom i) [0..i] | i <- [0..n]]

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myConcat :: [[a]] -> [a]
myConcat [xs] = xs
myConcat (x:xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n k = k : myReplicate (n-1) k

index :: [a] -> Int -> a
index (x:_) 0 = x
index (x:xs) i = index xs (i-1)

myElem :: Eq a => a -> [a] -> Bool
myElem n [] = False
myElem n (x:xs) | n == x = True
                | otherwise = myElem n xs

myConcat' :: [[a]] -> [a]
myConcat' xss = [i | l <- xss, i <- l]

myReplicate' :: Int -> a -> [a]
myReplicate' n k = [k | i <- [1..n]]

index' :: [a] -> Int -> a
index' xs i = head [k | (k, m) <- zip xs [0..length xs], i == m ]

strong :: String -> Bool
strong s = length s > 8 && any (\x-> isAlpha x && isUpper x) s
            && any (\x-> isAlpha x && isLower x) s
            && any isNumber s

divpropsqrt :: Integer -> [Integer]
divpropsqrt 1 = []
divpropsqrt 2 = [1]
divpropsqrt n = [i | i <- [1..ceiling (sqrt (fromIntegral n))], mod n i == 0]
mindiv :: Integer -> Integer
mindiv n | null l = -2
         | l == [1] = -1
         | otherwise = l !! 1
         where l = divpropsqrt n

prime' :: Integer -> Bool
prime' n = mindiv n == -1

myNub :: Eq a => [a] -> [a] 
myNub xs = nubAux xs []

nubAux :: Eq a => [a] -> [a] -> [a]
nubAux [] _ = []
nubAux (x:xs) uniques | elem x uniques = nubAux xs uniques
                      | otherwise = x:(nubAux xs (x:uniques))

myIntersperse :: a -> [a] -> [a]
myIntersperse s [] = []
myIntersperse s [x] = [x]
myIntersperse s (x:xs) = x:s:(myIntersperse s xs)

digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

toBits :: Int -> [Int]
toBits 0 = [0]
toBits 1 = [1]
toBits x = toBits(div x 2) ++ [mod x 2]

toBits' :: Integral a => a -> [a]
toBits' 0 = []
toBits' n = reverse (helper n)

helper :: Integral a => a -> [a]
helper 0 = []
helper n = mod n 2 : helper (div n 2)

fromBits :: [Int] -> Int 
fromBits xs = sum [(xs !! i)*2^(length xs -1 - i) | i <- [0..length xs -1]]

gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd b (mod a b)
