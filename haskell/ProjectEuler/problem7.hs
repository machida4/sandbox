-- 素数を小さい方から6つ並べると 2, 3, 5, 7, 11, 13 であり, 6番目の素数は 13 である.

-- 10 001 番目の素数を求めよ.

primeNumbers = 2 : 3 : [x | x <- [5, 7 ..], all (\p -> x `mod` p /= 0) (takeWhile (\p -> p * p <= x) primeNumbers)]

main = do print $ primeNumbers !! (10000)
