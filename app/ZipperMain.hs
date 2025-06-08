{-# LANGUAGE RecordWildCards #-}
module ReductionZipperSkeleton where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State (State)

-- 1. Rozbudowa AST ----------------------------------------------------------

-- Nazwy i wzorce

type Name = String

data Pat
  = PVar Name         -- zmienna wzorca
  | PApp Name [Pat]   -- konstruktor i lista wzorców
  deriving (Eq, Show)

-- Wyrażenia: zmienna, konstruktor, aplikacja
infixl 9 :$

data Expr
  = Var Name
  | Con Name
  | Expr :$ Expr
  deriving (Eq, Show)

-- 2. Zipper dla Expr --------------------------------------------------------

-- Crumb opisuje, jaką część drzewa utracił fokus:
-- AppLeft arg  = fokus na lewym poddrzewie aplikacji (_ :$ arg)
-- AppRight fun = fokus na prawym poddrzewie (fun :$ _)
data Crumb
  = AppLeft  Expr
  | AppRight Expr
  deriving (Eq, Show)

type Context = [Crumb]

type Zipper = (Expr, Context)

-- Schodzimy w lewo (na funkcję w aplikacji)
goDownLeft :: Zipper -> Maybe Zipper
goDownLeft (fun :$ arg, ctx) = Just (fun, AppLeft arg : ctx)
goDownLeft _                 = Nothing

-- Schodzimy w prawo (na argument w aplikacji)
goDownRight :: Zipper -> Maybe Zipper
goDownRight (fun :$ arg, ctx) = Just (arg, AppRight fun : ctx)
goDownRight _                  = Nothing

-- Wchodzimy w górę, rekonstruując aplikację z kontekstu
goUp :: Zipper -> Maybe Zipper
goUp (e, AppLeft arg : ctx)  = Just (e :$ arg, ctx)
goUp (e, AppRight fun : ctx) = Just (fun :$ e, ctx)
goUp (_, [])                  = Nothing

-- Odbudowujemy wyrażenie z całego zippera
rebuild :: Zipper -> Expr
rebuild (e, [])   = e
rebuild z         = case goUp z of
                      Just z' -> rebuild z'
                      Nothing -> error "rebuild: nieprawidłowy kontekst"

-- 3. Stan redukcji z historią i paliwem -------------------------------------

-- Zakładamy, że DefMap i typ Match są zdefiniowane w module parsera
-- DefMap mapuje nazwę funkcji na listę jej definicji (dopasowań)
type DefMap = Map Name [Match]

data Match = Match
  { matchName :: Name
  , matchPats :: [Pat]
  , matchRhs  :: Expr
  }

-- SnocList: sekwencja z doklejaniem na końcu w O(1)
newtype SnocList a = SnocList { unSnocList :: [a] }

-- Stan redukcji: definicje, pozostałe kroki (fuel), historia zipperów
data RState = RState
  { defs    :: DefMap
  , fuel    :: Int
  , history :: SnocList Zipper
  }

-- Monad stanu dla redukcji
-- Operacje na stanie będą dokładać kolejne zippery do historii i zmniejszać paliwo
type R a = State RState a
