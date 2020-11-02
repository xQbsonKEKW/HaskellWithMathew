{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
  ( NonEmpty
  , nonEmpty
  )
  where

newtype NonEmpty a = NonEmpty [a]
  deriving (Eq, Functor, Applicative, Monad, Foldable)

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty l = Just $ NonEmpty l