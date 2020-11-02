{-# LANGUAGE InstanceSigs, ScopedTypeVariables #-}
module Tree where

import Control.Monad.State

{-                 Lekcja 1                -}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node Leaf x Leaf
insert x (Node l y r) | x == y = Node l y r
insert x (Node l y r) =
  if x < y
    then Node (insert x l) y r
    else Node l y (insert x r)


union :: (Ord a) => Tree a -> Tree a -> Tree a
union l r = foldr insert r l

{-
Foldy

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [x1,x2,...,xn] = f( .. f (f (f acc x1) x2) x3) .. ) xn

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [x1,x2,...,xn] = f x1 (f x2 (f x3 (.. f xn acc) ) )

nieogonowa rekurencja
len :: [a] -> Int
len [] = 0
len _:xs = 1 + len xs

ogonowa rekurencja
len' :: [a] -> Int
len' l = go 0 l where
  go acc []     = acc
  go acc (_:xs) = go (acc+1) xs

foldl f acc l = case l of
  [] -> acc
  x:xs -> foldl f (f acc x) xs

foldr f acc l = case l of
  [] -> acc
  x:xs = f x (foldr f acc xs)

-}

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr f acc (Node l v r) = foldr f (f v (foldr f acc r)) l
{-
  foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl _ acc Leaf = acc
  foldl f acc (Node l v r) = foldl f (f (foldl f acc l) v) r
-}
---------------------------- łatwe -----------------------------------
size :: Tree a -> Int
size tree = size' tree 0 where
  size' Leaf n = n
  size' (Node l _ r) n = size' r (size' l (n+1))

height :: Tree a -> Int
height Leaf = 0
height (Node l _ r) = 1 + max (height l) (height r)

-- przykład jak działa foldr
toList :: Tree a -> [a]
toList = foldr (:) []

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

treesort :: (Ord a) => [a] -> [a]
treesort = toList . fromList

---------------------------- średnie ----------------------------------

-- | enumerowanie prefiksowo
prefix :: Tree a -> Tree (a, Int)
prefix tree = fst $ prefix' tree 1
  where
    -- prefix' :: Drzewo -> numer na którym zaczynamy -> (Drzewo, numer na którym skończyłem numerować)
    prefix' :: Tree a -> Int -> (Tree (a, Int), Int)
    prefix' Leaf n = (Leaf, n)
    prefix' (Node l v r) n = (Node l' (v, n) r', n'') where
      (l', n')  = prefix' l (n+1)
      (r', n'') = prefix' r  n'

-- | dla przykładu, pseudo - imperatywnie z do oraz State
prefixS :: Tree a -> Tree (a, Int)
prefixS tree = evalState (prefixS' tree) 1 -- ustawiamy początkowo zmienną pomocniczą na 1
  where
    prefixS' :: Tree a -> State Int (Tree (a, Int)) -- funkcja która zwraca Tree (a, Int) z pomocniczą zmienną typu Int
    prefixS' Leaf = pure Leaf
    prefixS' (Node l v r) = do
      n <- get  -- pobieramy stan zmiennej n
      modify (+1) -- dodajemy 1
      l' <- prefixS' l -- enumerujemy lewy (ze zmienionym stanem)
      r' <- prefixS' r -- enumerujemy prawy (ze zmienionym w poprzedniej linijce stanem)
      return $ Node l' (v, n) r'

-- | ścieżka wyszukiwania elementu x w drzewie (jakie wierzchołki dotykamy podczas insert)
path :: (Ord a) => a -> Tree a -> [a]
path x tree = path' x tree []
  where
    path' _ Leaf acc = acc
    path' x (Node l v _) acc | x < v     = path' x l (v:acc)
    path' x (Node _ v _) acc | x == v    = v:acc
    path' x (Node _ v r) acc | otherwise = path' x r (v:acc)

-- | znajduje element listy, dla którego wartość funkcji f jest minimalna (lub Nothing, jak lista jest pusta)
-- | > minimumOn id [] == Nothing
-- | > minimumOn length ["test","extra","a"] == Just "a"
safeMinimumOn :: (Ord b) => (a -> b) -> [a] -> Maybe a
safeMinimumOn _ [] = Nothing
safeMinimumOn f (x:xs) = Just $ go x (f x) xs
    where
        go v mv [] = v
        go v mv (x:xs) | mx < mv = go x mx xs
                       | otherwise = go v mv xs
            where mx = f x

-- | znajduje największy element w drzewie nie większy on podanego
-- | po wszystkich elementach na ścieżce wyszukiwania, wybierz nie większe od e i znajdź ten najbliżej e
lowerBound :: (Ord a, Num a) => a -> Tree a -> Maybe a
lowerBound e tree = safeMinimumOn (\x -> e - x) . filter (<= e) $ path e tree
-- można też w lambdzie napisać (e-)

-- | najmniejszy nie mniejszy od podanego
upperBound :: (Ord a, Num a) => a -> Tree a -> Maybe a
upperBound e tree = safeMinimumOn (\x -> x - e) . filter (>= e) $ path e tree

-- data Either a b = Left a | Right b

loop :: (a -> Either a b) -> a -> b
loop f x = case f x of
  Left y  {- y :: a -} -> loop f y
  Right z {- z :: b -} -> z

-- policzyć sumę elementów listy za pomocą loop
sumLoop :: [Int] -> Int
sumLoop lista = loop fun (0, lista)
  where
    fun :: (Int, [Int]) -> Either (Int, [Int]) Int
    fun (acc, []) = Right acc
    fun (acc, x:xs) = Left (x+acc, xs)

--napisać folda za pomocą loopa

foldlLoop :: forall a b . (b -> a -> b) -> b -> Tree a -> b
foldlLoop f acc t = loop loopf $ (acc, [Right t])
  where
    -- ScopedTypeVariables i forall są po to, by GHC wyczaił że to a i b poniżej to te same co te powyżej
    loopf :: (b, [Either a (Tree a)]) -> Either (b, [Either a (Tree a)]) b
    loopf (acc, []) = Right acc
    loopf (acc, t:ts) = case t of
      Left val -> Left (f acc val, ts)
      Right Leaf -> Left (acc, ts)
      Right (Node l v r) -> Left (acc, Right l : Left v : Right r : ts)



{-                    Lekcja 2                   -}

-- 1. Napisać maximum, minimum, sumę, iloczyn (elementów listy) foldem
-- maximum :: (Ord a) => [a] -> Maybe a

-- 2. Napisać loopem funkcję concat :: [[a]] -> [a]
-- [[1,2], [3,4], [5]] = [1,2,3,4,5]
-- A potem foldem: concat' :: Foldable t => t [a] -> [a]

-- 4 (na pomyślenie) zastanowić się, jak mogłaby wyglądać implementacja funkcji
-- ap :: Set (a -> b) -> Set a -> Set b
-- najpierw napisać instancję Functor dla seta

-- 3. napisać merge na listach, tj. dla posortowanych l i r zwróci posortowaną listę elementów z l i r
-- merge :: (Ord a) => [a] -> [a] -> [a]
-- merge [1,5,7] [2,4,8] = [1,2,4,5,7,8]

