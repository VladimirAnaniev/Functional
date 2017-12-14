main :: IO()
main = do
  print "yo"

maximize :: [(Int -> Int)] -> (Int -> Int)
maximize xs = \x -> max xs x x
  where
  max :: [(Int -> Int)] -> Int -> (Int -> Int)
  max xs x = foldl (\fun acc -> if abs (fun x) > abs (acc x) then fun else acc) (\x -> 0) xs

digits :: Int -> [(Int)]
digits n
  | n < 10 = [n]
  | otherwise = (digits (n `div` 10)) ++ [(n `mod` 10)]

times :: Int -> Int -> [Int]
times x 0 = []
times x n = x:(times x (n - 1))

collatz :: Int -> Int
collatz 1 = 1
collatz n
  | even n = div n 2
  | otherwise = 3*n + 1

areCollatzSeq :: [Int] -> Bool
areCollatzSeq (x:[]) = True
areCollatzSeq xs@(x:y:_) = if collatz x == y then areCollatzSeq (tail xs) else False

--TODO
--merge :: Ord a => [a] -> [a] -> [a]
--merge xs ys
--  | firstx < firstY

rle :: [Char] -> [(Char, Int)]
rle [] = []
rle xs = helper (head xs) 1 (tail xs)
  where
  helper :: Char -> Int -> [Char] -> [(Char, Int)]
  helper last n (x:xs)
    | last == x = helper x (n + 1) xs
    | otherwise = (last, n):helper x 1 xs
  helper last n [] = [(last, n)]
