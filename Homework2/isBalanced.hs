main :: IO()
main = do
  print "123"

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Read, Show)
