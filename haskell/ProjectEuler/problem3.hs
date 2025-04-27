-- 13195 の素因数は 5, 7, 13, 29 である.

-- 600851475143 の素因数のうち最大のものを求めよ.

smallestPrimeFactor :: Integer -> Integer
smallestPrimeFactor n = head [k | k <- [2 .. n], n `mod` k == 0]

target :: Integer
target = 600851475143

largestPrimeFactor n =
  let p = smallestPrimeFactor n
   in if p == n
        then p
        else largestPrimeFactor $ n `div` p

main = do
  print $ largestPrimeFactor target
