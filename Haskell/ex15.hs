import Data.List

main :: IO()
main = do
  print "123"

data NestedList a = Empty | Elem a | List [NestedList a]

testList = (List [
               (Elem 2),
               Empty,
               (Elem 3),
               (List [
                     (Elem 4),
                     (Elem 5),
                     Empty,
                     (List [
                           (Elem 66),
                           Empty,
                           (Elem 33)
                           ])
                     ]),
               (Elem 6)
               ])

flatten :: NestedList a -> [a]
flatten Empty = []
flatten (Elem a) = [a]
flatten (List xs) = foldr (\x acc -> (flatten x) ++ acc) [] xs

elemsOnLevel :: NestedList a -> Int -> [a]
elemsOnLevel Empty _ = []
elemsOnLevel (Elem x) 0 = [x]
elemsOnLevel (Elem _) _ = []
elemsOnLevel _ 0 = []
elemsOnLevel (List xs) n = foldr (\x acc -> (elemsOnLevel x (n - 1)) ++ acc) [] xs

assignJobs :: [[Int]] -> Int
assignJobs = getMax . combinations
  where
    combinations :: [[Int]] -> [[Int]]
    combinations = foldr (\x acc -> [y:ys | y <- x, ys <- acc]) [[]]

    getMax :: [[Int]] -> Int
    getMax = foldr (\x acc -> max (length x) acc) 0 . map nub

findMinTime :: Int -> [Int] -> Int
findMinTime k ds = foldr1 min [] (map max (combinations ds k))
  where
    combinations :: [Int] -> Int -> [[Int]]
    --TODO
