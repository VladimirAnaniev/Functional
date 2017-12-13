main:: IO()
main = do
  print(prime 3)

count_digits :: Int -> Int
count_digits n
  | n < 10 = 1
  | otherwise = 1 + (count_digits (n `div` 10))

sum_digits :: Int -> Int
sum_digits 0 = 0
sum_digits n = (n `mod` 10) + sum_digits (n `div` 10)

sum_digits_iter:: Int -> Int
sum_digits_iter n = iter n 0
  where
  iter :: Int -> Int -> Int
  iter 0 sum = sum
  iter n sum = iter (n `div` 10) (sum + (n `mod` 10))
  
prime :: Int -> Bool
prime n 
  | n <= 1 = False
  | n == 2 = True
  | otherwise = check 2
    where 
    check :: Int -> Bool
    check i  
      | (n `mod` i) == 0 = False
      | (fromIntegral i) > sqrt (fromIntegral n) = True
      | otherwise = check (i + 1)

pow :: Double -> Int -> Double
pow _ 0 = 1
pow x n = x * (pow x (n - 1))

rev :: Int -> Int
rev n = reverse n 0 ((count_digits n) - 1)
  where
  reverse :: Int -> Int -> Int -> Int
  reverse 0 result _ = result
  reverse remaining result power = reverse (remaining `div` 10) (((remaining `mod` 10) * (10 ^ power)) + result) (power - 1)


  
  
