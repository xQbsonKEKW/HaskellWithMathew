module Main where

import Prelude hiding (sum, reverse)
import Set
import Tree

-- data List a = Empty | List a (List a)
-- data [a] = [] | a : [a]
--JS: kodze na kompie z windowsem XP - dostaję raka xD

nub :: (Eq a) => [a] -> [a]
nub = reverse . Set.toList . Set.fromList

reverse :: [a] -> [a]
reverse l = reverse' [] l
  where
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs

main :: IO ()
main = print $ Tree.prod [1,3,5,1,2,2,6,8,5]