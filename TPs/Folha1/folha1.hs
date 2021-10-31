testTriangle :: Float -> Float -> Float -> Bool
testTriangle a b c = a < (b+c)

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c | testTriangle a b c = sqrt (s*(s-a)*(s-b)*(s-c))
                   | otherwise = -1
                   where s = (a+b+c)/2.0

halves :: [a] -> ([a], [a])
halves xs = (take half xs, drop half xs) where half = div (length xs) 2

last1, last2, last3 :: [a] -> a
last1 xs = head (reverse xs)
last2 xs = xs !! (length xs - 1)
last3 xs = head (drop (length xs - 1) xs)

init1, init2 :: [a] -> [a]
init1 xs = take (length xs - 1) xs
init2 xs = reverse (drop 1 (reverse xs))

fact :: Integer -> Integer
fact n = product [1..n]
binom :: Integer -> Integer -> Integer
binom n k = div (fact n) ( fact k * fact (n-k) )

binom' :: Integer -> Integer -> Integer
binom' n k | k < n - k = div (product [n-k+1 .. n]) (fact k)
           | otherwise = div (product [k+1 .. n]) (fact (n-k))

roots :: Float -> Float -> Float -> (Maybe Float, Maybe Float)
roots a b c | delta > 0 = ( Just ((-b+sqrt delta)/(2*a)), Just ((-b-sqrt delta)/(2*a)) )
            | delta == 0 = ( Just (-b/(2*a)), Nothing )
            | otherwise = ( Nothing, Nothing )
            where delta = b^2 - 4*a*c

{-
['a', 'b', 'c'] -> [Char]
('a', 'b', 'c') -> (Char, Char, Char)
[(False, '0'), (True, '1')] -> [(Bool, Char)]
([False, True], ['0', '1']) -> ([Bool], [Char])
[tail, init, reverse] -> [[a]->[a]]
[id, not] -> [Bool -> Bool]
-}

{-
a) [a] -> a
b) (a,b) -> (b,a)
c) a-> b -> (a,a)
d) Num a => a-> a
e) Fractional a => a-> a
f) Char -> Bool
g) Ord a => a -> a -> a -> Bool
h) Eq a => [a] -> Bool
i) (a -> a) -> a -> a
-}

grade :: Int -> String
grade n | n >= 0  && n <= 9 = "reprovado"
        | n >= 10 && n <= 12 = "suficiente"
        | n >= 13 && n <= 15 = "bom"
        | n >= 16 && n <= 18 = "muito bom"
        | n >= 19 && n <= 20 = "muito bom com distinção"
        | otherwise = "erro"

classify :: (Ord a, Fractional a) => a -> a -> String
classify w h | imc > 0 && imc < 18.5 = "baixo peso"
             | imc >= 18.5 && imc < 25 = "peso normal"
             | imc >= 25 && imc < 30 = "excesso de peso"
             | imc >= 30 = "obesidade"
             | otherwise = "erro"
              where imc = w/h^2

max3, min3 :: Ord a => a -> a -> a -> a
max3 a b c | a > b && b > c = a
           | b > a && a > c = b
           | otherwise = c
min3 a b c | a <= b && b <= c = a
           | b <= a && a <= c = b
           | otherwise = c
           
max3', min3' :: Ord a => a -> a -> a -> a
max3' a b = max (max a b)
min3' a b = max (max a b)

xor, xor', xor'' :: Bool -> Bool -> Bool
xor False False = False 
xor True True = False
xor True False = True
xor False True = True
xor' x y | x == y = False
xor' _ _ = True
xor'' True x = not x
xor'' False x = x

safetail, safetail' :: [a] -> [a] 
safetail [] = []
safetail xs = tail xs
safetail' xs = case xs of
                [] -> []
                xs -> tail xs

short :: [a] -> Bool
short xs = length xs < 3

short' :: [a] -> Bool
short' [] = True
short' [_] = True
short' [_,_] = True
short' xs = False 
{- short' _ = False -}

median :: Ord a => a -> a -> a -> a
median a b c | b > a && a > c || (b < a && a < c) = a
             | a > b && b > c || (a < b && b < c) = b
             | otherwise = c

median' :: (Num a, Ord a) => a -> a -> a -> a
median' a b c = a + b + c - max3 a b c - min3 a b c 

convert :: Int -> String
convert _ = "hi"