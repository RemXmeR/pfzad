{-# LANGUAGE InstanceSigs #-}

module SnocList
  ( SnocList
  , toList
  , fromList
  , snoc
  ) where

import Data.Semigroup  (Semigroup(..))
import Control.Applicative (Alternative(..), Applicative(..))

-- | Sekwencja z doklejaniem na końcu w czasie stałym.
newtype SnocList a = SnocList { unSnocList :: [a] }

-- | Zamienia 'SnocList' na zwykłą listę.
toList :: SnocList a -> [a]
toList = unSnocList

-- | Tworzy 'SnocList' z listy.
fromList :: [a] -> SnocList a
fromList = SnocList

-- | Dokleja element na koniec.
snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (xs ++ [x])

-- Eq i Show przez delegację na listę
instance Eq a => Eq (SnocList a) where
  (==) :: SnocList a -> SnocList a -> Bool
  SnocList xs == SnocList ys = xs == ys

instance Show a => Show (SnocList a) where
  show :: SnocList a -> String
  show (SnocList xs) = show xs

-- Semigroup i Monoid przez konkatenację
instance Semigroup (SnocList a) where
  (<>) :: SnocList a -> SnocList a -> SnocList a
  SnocList xs <> SnocList ys = SnocList (xs ++ ys)

instance Monoid (SnocList a) where
  mempty  :: SnocList a
  mempty = SnocList []

  mappend :: SnocList a -> SnocList a -> SnocList a
  mappend = (<>)

-- Functor przez map
instance Functor SnocList where
  fmap :: (a -> b) -> SnocList a -> SnocList b
  fmap f (SnocList xs) = SnocList (map f xs)

-- Applicative: pure jako singleton; <*> jako kombinacja wszystkich par
instance Applicative SnocList where
  pure :: a -> SnocList a
  pure x = SnocList [x]

  (<*>) :: SnocList (a -> b) -> SnocList a -> SnocList b
  SnocList fs <*> SnocList xs =
    SnocList [ f x | f <- fs, x <- xs ]

-- Alternative: empty jako pusty, <|> jako konkatenacja
instance Alternative SnocList where
  empty :: SnocList a
  empty = SnocList []

  (<|>) :: SnocList a -> SnocList a -> SnocList a
  (<|>) = (<>)
