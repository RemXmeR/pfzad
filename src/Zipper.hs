module Zipper
  ( ExprZ
  , Context(..)
  , goLeft
  , goRight
  , goUp
  , goTop
  ) where

import AST (Expr(..))

-- | Jeden krok kontekstu: albo jesteśmy w pozycji funkcji (InFunc arg),
-- albo w pozycji argumentu (InArg func).
data Context
  = InFunc Expr    -- [ ] :$ arg
  | InArg  Expr    -- func :$ [ ]
  deriving (Eq, Show)

-- | Skupienie: bieżące wyrażenie plus stos ramek kontekstu
type ExprZ = (Expr, [Context])

-- | Ze skupienia na aplikacji f :$ a schodzimy na lewo (do funkcji f).
goLeft :: ExprZ -> Maybe ExprZ
goLeft (f :$ a, ctx) = Just (f, InFunc a : ctx)
goLeft _             = Nothing

-- | Ze skupienia na aplikacji f :$ a schodzimy na prawo (do argumentu a).
goRight :: ExprZ -> Maybe ExprZ
goRight (f :$ a, ctx) = Just (a, InArg f : ctx)
goRight _             = Nothing

-- | Wracamy z ramki: jeśli ostatnia to InFunc a, to odbudowujemy f :$ a;
-- jeśli InArg f, to f :$ e.
goUp :: ExprZ -> Maybe ExprZ
goUp (e, InFunc a : ctx) = Just (e :$ a, ctx)
goUp (e, InArg  f : ctx) = Just (f :$ e, ctx)
goUp (_, [])             = Nothing

-- | Przechodzimy na samą górę drzewa, podążając za goUp aż do pustego kontekstu.
goTop :: ExprZ -> ExprZ
goTop ez = case goUp ez of
  Just ez' -> goTop ez'
  Nothing  -> ez
