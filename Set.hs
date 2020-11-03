{-# LANGUAGE InstanceSigs #-}

module Set where

import Control.Applicative
import Control.Monad

data Set a = Empty | Insert a (Set a)

instance Functor Set where
  fmap :: (a -> b) -> Set a -> Set b
  fmap = undefined

instance Applicative Set where
  pure :: a -> Set a 
  pure x = Insert x Empty 

  (<*>) :: Set (a -> b) -> Set a -> Set b
  fs <*> xs = undefined
--JS: co jest
{-
MG: próbuję przebić się jakoś przez fakt, że Set jako taki nie jest monadą
ani Applicative, bo nie możemy żądać (Eq a) w instancji.
@see: http://okmij.org/ftp/Haskell/set-monad.html
Ale chyba się nie da :(
-}

instance Monad Set where 
  (>>=) :: Set a -> (a -> Set b) -> Set b
  xs >>= f = undefined
  

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

unions :: (Eq a) => [Set a] -> Set a 
unions = undefined

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