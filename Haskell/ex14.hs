data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)

getValue :: Tree a -> Maybe a
getValue Empty = Nothing
getValue (Node x _ _) = Just x

listSpecial :: (Eq a, Num a) => Tree a -> [a]
listSpecial Empty = []
listSpecial (Node x left right)
  | x == nodesSum left right = x:listSpecial left ++ listSpecial right
  | otherwise = listSpecial left ++ listSpecial right
  where
    nodesSum :: (Eq a, Num a) => Tree a -> Tree a -> a
    nodesSum Empty Empty = 0
    nodesSum Empty (Node second _ _) = second
    nodesSum (Node first _ _) Empty = first
    nodesSum (Node first _ _) (Node second _ _) = first + second

areMirrorImages :: (Eq a) => Tree a -> Tree a -> Bool
areMirrorImages Empty Empty = True
areMirrorImages (Node x l1 r1) (Node y l2 r2) = x == y && areMirrorImages l1 r2 && areMirrorImages l2 r1
areMirrorImages _ _ = False

isBST :: (Ord a) => Tree a -> Bool
isBST Empty = True
isBST (Node x left right) = check x lval rval && isBST left && isBST right
  where
    lval = getValue left
    rval = getValue right
    check :: (Ord a) => a -> Maybe a -> Maybe a ->  Bool
    check curr Nothing (Just b) = curr < b
    check curr (Just a) Nothing = curr > a
    check curr (Just a) (Just b) = a < curr && curr < b
    check _ _ _ = True

bstToList :: (Ord a) => Tree a -> [a]
bstToList Empty = []
bstToList (Node x l r) = bstToList l ++ [x] ++ bstToList r

bstSearch :: (Ord a) => Tree a -> a -> Bool
bstSearch Empty _ = False
bstSearch (Node x l r) y = x == y || bstSearch l y || bstSearch r y

bstInsert :: (Ord a) => Tree a -> a -> Tree a
bstInsert Empty y = Node y Empty Empty
bstInsert (Node x l r) y
  | x < y = Node x l (bstInsert r y)
  | otherwise = Node x (bstInsert l y) r

bstRemove :: (Ord a) => Tree a -> a -> Tree a
bstRemove Empty _ = Empty
bstRemove (Node x l r) y
  | x > y = Node x l (bstRemove r y)
  | x < y = Node x (bstRemove l y) r
  | otherwise = append l r
  where
    append :: (Ord a) => Tree a -> Tree a -> Tree a
    append Empty right = right
    append left Empty = left
    append (Node x l1 r1) right@(Node y l2 r2)
      | x > y = Node x (append l1 right) r1
      | otherwise = Node x l1 (append r1 right)

main :: IO()
main = let
    tree11 = (Node 3
                (Node 1
                    (Node 1 Empty Empty)
                    Empty)
                (Node 2
                    (Node 4 Empty Empty)
                    (Node 7 Empty Empty)))
    tree21 = (Node 4
                (Node 3
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty))
                (Node 5
                    Empty
                    (Node 6 Empty Empty)))
    tree22 = (Node 4
                (Node 5
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3
                    (Node 2 Empty Empty)
                    (Node 1 Empty Empty)))
    tree23 = (Node 4
                (Node 5
                    (Node 6 Empty Empty)
                    Empty)
                (Node 3
                    (Node 1 Empty Empty)
                    (Node 2 Empty Empty)))
    tree31 = (Node 4
                (Node 2
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5
                    Empty
                    (Node 6 Empty Empty)))
    tree32 = (Node 4
                (Node 2
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5
                    (Node 6 Empty Empty)
                    Empty))
    -- Зад. 4-7.
    tree   = (Node 4
                (Node 2
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5
                    Empty
                    (Node 6 Empty Empty)))
    in do
        -- Задача 1.
        print $ listSpecial tree11

        -- Задача 2.
        print $ areMirrorImages tree21 tree22
        print $ areMirrorImages tree21 tree23

        -- Задача 3.
        print $ isBST tree31
        print $ isBST tree32

        -- Задача 4.
        print $ bstToList tree

        -- Задача 5.
        print $ bstSearch tree 1
        print $ bstSearch tree 4
        print $ bstSearch tree 7

        -- Задача 6.
        print $ bstInsert tree 7

        -- Задача 7.
        print $ bstRemove tree 4
