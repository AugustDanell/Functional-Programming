sum' :: (Num a) => a -> a -> a
sum' x y = x + y

differance :: (Num a) => a -> a -> a
differance x y = x - y

product' :: (Num a) => a -> a -> a
product' x y = x * y

{-
division :: (Num a) => a -> a -> a
division x y = a / b
where a = fromIntegral x
      b = fromIntegral y
-}

recursiveFaculty :: (Integral a) => a -> a
recursiveFaculty 1 = 1
recursiveFaculty n = n * recursiveFaculty(n-1)

-- Given that arg 1 is the greater of the two.
greatestCD :: (Integral a) => a -> a -> a
greatestCD maxV minV = getGCD maxV maxV minV

getGCD :: (Integral a) => a -> a -> a -> a
getGCD arg maxV minV 
  | (maxV `mod` arg) == 0 && (minV `mod` arg) == 0 = arg
  | otherwise = getGCD (arg-1) maxV minV

-- leastCM ::

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = primeHelper 2 n

primeHelper :: Integer -> Integer -> Bool
primeHelper i n
  | (n `mod` i) == 0 = True
  | i == n - 1 = False
  | otherwise = primeHelper (i+1) n

