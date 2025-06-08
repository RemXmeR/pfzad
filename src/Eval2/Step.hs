{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-- src/Eval2/Step.hs
module Eval2.Step
  ( rstepM
  , rpathM
  ) where

import qualified Data.Map           as Map
import           AST
import           Eval1              (rstep, flattenApp, matchArgs)
import           SnocList           (SnocList, snoc)
import           Zipper
import           Eval2.State        (RState(..), R)
import           Control.Monad.State (get, put)
import           Control.Applicative ((<|>))
import           Data.Maybe (isJust)
import           Control.Monad      (foldM)

-- | Czy wzorzec to zmienna?
isPVar :: Pat -> Bool
isPVar (PVar _) = True
isPVar _        = False

-- | Ze skupienia na (f :$ a1 :$ a2 :$ … :$ an) schodzę do i-tego argumentu ai.
goToArg :: Int -> ExprZ -> Maybe ExprZ
goToArg i ez0 = do
  let (e, ctx) = ez0
      (_, args) = flattenApp e
      n = length args
  -- nie wychodzić poza zakres
  if i < 0 || i >= n then Nothing else do
    -- ile razy goLeft? (n-1-i) razy:
    ez1 <- foldM (\ez _ -> goLeft ez) ez0 [1..(n-1-i)]
    -- potem jeden goRight, by wejść do ai
    goRight ez1

-- | Szuka kolejnego redexa według „normal order, lazy pattern‐match”:
findRedex :: DefMap -> ExprZ -> Maybe ExprZ
findRedex defs ez@(e, ctx) =
  case flattenApp e of
    (Var f, args) | Just matches <- Map.lookup f defs ->
      -- najpierw tylko te klauzule, które mają wzorzec konstruktorowy (PApp)
      let specific = filter (any (not . isPVar) . matchPats) matches in
      case specific of
        -- jeśli są wzorce konstruktorowe, schodzimy do pierwszego argumentu,
        -- który ich wymaga, i tam szukamy redexa
        (m:_) ->
          let ps      = matchPats m
              -- indeksy wzorców PApp
              idxs    = [ i | (pat,i) <- zip ps [0..], not (isPVar pat) ]
          in case idxs of
               (i:_) -> do
                 ezArg <- goToArg i ez
                 findRedex defs ezArg
               [] ->
                 -- niespodziewane, ale gdy brak PApp mimo specific/=[], fallback
                 Just ez

        -- gdy nie ma wzorców konstruktorowych, root jest catch‐all albo zwykłym redexem
        [] -> Just ez

    -- w każdej innej sytuacji wędruj lewo/prawo
    _ ->
      (goLeft ez  >>= findRedex defs)
   <|> (goRight ez >>= findRedex defs)

-- | Jeden krok redukcji w monadzie: najpierw szukamy redexa „głęboko”,
--   wykonujemy `rstep` **po** redukcji dopisujemy stan, a potem odbudowujemy do korzenia.
rstepM :: ExprZ -> R (Maybe ExprZ)
rstepM ez0 = do
  st@RState{ defs, fuel, history } <- get
  case findRedex defs ez0 of
    Nothing      -> return Nothing
    Just ezRedx  -> do
      let (redx, ctx) = ezRedx
          Just redx'  = rstep defs redx
          ezAfter     = goTop (redx', ctx)
      put st
        { fuel    = fuel - 1
        , history = snoc history ezAfter
        }
      return (Just ezAfter)

-- | Cała ścieżka redukcji (zaznaczamy każdy stan po redukcji).
rpathM :: ExprZ -> R ()
rpathM ez = do
  mez <- rstepM ez
  case mez of
    Just ez' -> rpathM ez'
    Nothing  -> return ()