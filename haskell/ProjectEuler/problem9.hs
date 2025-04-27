ans :: Integer
ans = head [a * b * (1000 - a - b) | a <- [1 .. 1000], b <- [a + 1 .. 1000], isIntegerRightTriangle a b]

isIntegerRightTriangle :: (Ord a, Num a) => a -> a -> Bool
isIntegerRightTriangle a b =
  a < b
    && b < c
    && a * a + b * b == c * c
  where
    c = 1000 - a - b

main = do print ans
