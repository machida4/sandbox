-- 10以下の素数の和は 2 + 3 + 5 + 7 = 17 である.

-- 200万以下の全ての素数の和を求めよ.

primeNumbers = 2 : 3 : [x| x <- [5, 7 ..], all (\p -> x `mod` p /= 0) (takeWhile (\q -> q * q <= x) primeNumbers)]
main = do
  print $ sum $ takeWhile (< 2000000) primeNumbers
