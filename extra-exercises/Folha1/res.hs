{- IN3
a) (3 - (-2)) + 1
b) (4 / (-2)) - (3 * 6)
c) ((-) 2 3) * 6
d) (100 `div` 4) `div` 3
e) 100 `div` (div 4 3)
f) ((+) 1 2) + (3 * 4)
g) (+)((5 `mod` 2) + 2)(mod 5 2)
-}

{- IN4
72 / 9 + (-)5 4 * ((-)14 7 / 7)
(72 / 9) + ((-)5 4) * ( ((-)14 7) / 7)


                   +    
          /                 *
       72   9         -            /
                    5   4       -     7
                             14   7
-}

half :: Fractional a => a -> a
half x = x/2

xor :: Bool -> Bool -> Bool
xor x y | x == y = x
xor _ _ = True

cbrt :: Floating a => a -> a
cbrt x = x**(1/3)

isTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isTriangle a b c = a < (b+c)

heron :: (Floating a, Ord a) => a -> a -> a -> a
heron a b c | isTriangle a b c = let s = (a+b+c)/2 in
                                 sqrt(s*(s-a)*(s-b)*(s-c))
            | otherwise = 0.0