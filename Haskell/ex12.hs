import Data.Char
import Data.List

main :: IO()
main = do
  print $ filterPrimePositions [1, 2, 3, 4, 5]
  print $ countMinimum [1, 2, 1, 3, 1, 4]
  print $ images (\x -> x * x) (\x -> x + 2) [(2, 2), (1, 2), (3, 7)]
  print $ title "the souND aND tHe fuRY"
  print $ isIterator (+2) [1, 3, 5]

filterPrimePositions :: [Int] -> [Int]
filterPrimePositions xs = [n | (i, n) <- list, isPrime i]
  where
  list = zip [2..] xs
  isPrime :: Int -> Bool
  isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

countMinimum :: [Int] -> Int
countMinimum xs = length [n | n <- xs, n == minimum xs]

title :: String -> String
title [] = []
title str = toUpper (head str):titleCase str
  where
  titleCase [] = []
  titleCase strr@(' ':y:_) = toUpper y:titleCase (tail strr)
  titleCase (_:y:[]) = toLower y:[]
  titleCase strr@(_:y:_) = toLower y:titleCase (tail strr)
  titleCase (x:_) = toUpper x:[]

images :: (Double -> Double) -> (Double -> Double) -> [(Double, Double)] -> [(Double, Double)]
images f g xys = [(x, y) | (x, y) <- xys, f x == g y]

isIterator :: (Eq a) => (a -> a) -> [a] -> Bool
isIterator _ [] = True
isIterator _ (_:[]) = True
isIterator f xs@(x:y:_) = if f x == y then isIterator f (tail xs) else False

-- isPangram :: String -> Bool TODO
