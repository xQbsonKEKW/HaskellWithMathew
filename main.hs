module Main where

import Prelude hiding (sum, reverse)
import Set

-- data List a = Empty | List a (List a)
-- data [a] = [] | a : [a]


nub :: (Eq a) => [a] -> [a]
nub = reverse . toList . fromList

reverse :: [a] -> [a]
reverse l = reverse' [] l
  where
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs

main :: IO ()
main = print $ nub [1,3,5,1,2,2,6,8,5]