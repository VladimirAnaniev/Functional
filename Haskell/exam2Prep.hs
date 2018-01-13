import Data.List

main :: IO()
main  = do
  print "123"

reverseOrdStuff :: Int -> Int
reverseOrdStuff = helper []
  where
    helper :: [Int] -> Int -> Int
    helper [] num = helper [num `mod` 10] (num `div` 10)
    helper arr 0 = concat arr
    helper arr num
      | num `mod` 10 > (head arr) = helper ((num `mod` 10):arr) (num `div` 10)
      | otherwise = helper arr 0

    concat :: [Int] -> Int
    concat [n] = n
    concat (x:xs) = (concat xs) * 10 + x


sumUnique :: [[Int]] -> Int
sumUnique = sum . map (sum . unique)
  where
    unique :: [Int] -> [Int]
    unique xs = [x | x <- xs, length [y | y <- xs, y == x] == 1]

maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize fs = (\x -> (foldr (\f acc -> if abs(f x) > abs(acc x) then f else acc ) (\_ -> 0) fs) x)

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = all (\x -> f(g x) == x && g(f x) == x) [a..b]
