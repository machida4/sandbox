-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
triangularNumbers :: [Integer]
triangularNumbers = 1 : zipWith (+) triangularNumbers [2 ..]

factorCount :: Integer -> Int
factorCount n =
  let limit = floor (sqrt (fromInteger n :: Double))
      count = sum [if d * d == n then 1 else 2 | d <- [1 .. limit], n `mod` d == 0]
   in count

main = do
  print $ head $ filter (\n -> factorCount n > 500) triangularNumbers
