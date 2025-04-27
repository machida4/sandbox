import Data.Char
import Data.List

divideByTen = (/ 10)

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 999999 ..])
  where
    p x = x `mod` 3829 == 0

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstToX :: Int -> Maybe Int
firstToX x = find (\i -> digitSum i == x) [1 ..]

main = do
  print $ divideByTen 100
  print $ map (* 10) [1 .. 20]
  print $ largestDivisible
  print $ firstToX 30
