import Prelude hiding (minimum, maximum, sum, product, length, and, or, any, all, concat, reverse)

main :: IO()
main = do print "opa"

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

length :: Num b => [a] -> b
length = foldr (\_ len -> len + 1) 0

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x acc -> p x || acc) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x acc -> p x && acc) True

minimum :: Ord a => [a] -> a
minimum (x:xs) = foldr (min) x xs

maximum :: Ord a => [a] -> a
maximum (x:xs) = foldr (max) x xs

concat :: [[a]] -> [a]
concat = foldr (++) []

reverse :: [a] -> [a]
reverse = foldl (\acc x -> x:acc) []

compose :: [(a -> a)] -> (a -> a)
compose = foldr (.) id

combinations :: [[a]] -> [[a]]
combinations = foldr (\xs acc -> [x : a | x <- xs, a <- acc]) [[]]
