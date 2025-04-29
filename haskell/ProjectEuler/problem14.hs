genCollaz :: (Integral t) => t -> [t]
genCollaz n
  | n == 1 = [1]
  | otherwise = if even n then n : genCollaz (n `div` 2) else n : genCollaz (3 * n + 1)

main = do
  print $ maximum $ map (length . genCollaz) [1 .. 1000000]
