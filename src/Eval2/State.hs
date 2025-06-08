-- src/Eval2/State.hs
module Eval2.State
  ( RState(..)
  , R
  , initState
  ) where

import AST               (DefMap)
import SnocList          (SnocList, fromList)
import Zipper            (ExprZ)
import Control.Monad.State

-- | Stan redukcji:
--   * defs    – mapa definicji
--   * fuel    – pozostała liczba kroków (fuel == 0 → przerwij redukcję)
--   * history – zapamiętane kolejne stany (Expr z kontekstem)
data RState = RState
  { defs    :: DefMap
  , fuel    :: Int
  , history :: SnocList ExprZ
  }

-- | Monad rozszerzony o stan RState
type R = State RState

-- | Tworzy początkowy stan redukcji dla danej mapy definicji i limitu kroków
initState :: DefMap   -- ^ definicje funkcji/konstruktorów
          -> Int      -- ^ maksymalna liczba kroków ("paliwo")
          -> RState
initState defMap maxFuel = RState
  { defs    = defMap
  , fuel    = maxFuel
  , history = fromList []
  }
