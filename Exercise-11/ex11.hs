main :: IO()
main = do
  print "yo"

maximize :: [(Int -> Int)] -> (Int -> Int)
maximize xs = \x -> maxi xs x x
  where
  maxi :: [(Int -> Int)] -> Int -> (Int -> Int)
  maxi fs x = foldl (\fun acc -> if abs (fun x) > abs (acc x) then fun else acc) (\_ -> 0) fs

digits :: Int -> [(Int)]
digits n
  | n < 10 = [n]
  | otherwise = (digits (n `div` 10)) ++ [(n `mod` 10)]

times :: Int -> Int -> [Int]
times _ 0 = []
times x n = x:(times x (n - 1))

collatz :: Int -> Int
collatz 1 = 1
collatz n
  | even n = div n 2
  | otherwise = 3*n + 1

arecollatzseq :: [Int] -> Bool
arecollatzseq [] = True
arecollatzseq (_:[]) = True
arecollatzseq xs@(x:y:_) = if collatz x == y then arecollatzseq (tail xs) else False

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(fstx:restx) ys@(fsty:resty)
  | fstx < fsty = fstx:merge restx ys
  | otherwise = fsty:merge xs resty

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort first) (mergesort second)
  where
  first = fst splitAtHalf
  second = snd splitAtHalf
  splitAtHalf = splitAt (((length xs) + 1) `div` 2) xs

rle :: [Char] -> [(Char, Int)]
rle [] = []
rle xs = helper (head xs) 1 (tail xs)
  where
  helper :: Char -> Int -> [Char] -> [(Char, Int)]
  helper lst n (x:rest)
    | lst == x = helper x (n + 1) rest
    | otherwise = (lst, n):helper x 1 rest
  helper lst n [] = [(lst, n)]
