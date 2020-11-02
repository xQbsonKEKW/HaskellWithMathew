module Set where

data Set a = Empty | Insert a (Set a)

empty :: Set a
empty = Empty

insert :: (Eq a) => a -> Set a -> Set a
insert v Empty = Insert v Empty
insert v (Insert x xs) = if x == v
  then Insert x xs
  else Insert x (insert v xs)

union :: (Eq a) => Set a -> Set a -> Set a
union Empty s = s
union (Insert x xs) ys = union xs (insert x ys)

delete :: (Eq a) => a -> Set a -> Set a
delete a Empty = Empty
delete a (Insert b bs) = if a == b
  then bs
  else Insert b (delete a bs)

toList :: Set a -> [a]
toList Empty = []
toList (Insert x xs) = x : toList xs

fromList :: (Eq a) => [a] -> Set a
fromList [] = Empty
fromList (x:xs) = insert x (fromList xs)