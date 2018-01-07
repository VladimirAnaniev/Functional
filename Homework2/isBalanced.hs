main :: IO()
main = print "5"

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Read, Show)

isBalanced :: Tree a -> Int -> Bool
isBalanced Empty _ = True
isBalanced (Node _ left right) k = check && isBalanced left k && isBalanced right k
  where
    check :: Bool
    check = abs((depth left) - (depth right)) <= k

    depth :: Tree a -> Int
    depth Empty = 0;
    depth (Node _ l r) = 1 + max (depth l) (depth r)
