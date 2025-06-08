{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment   (getArgs)
import qualified Data.Map    as Map
import Control.Monad.State  (execState, modify)
import AST
import Parser               (fromHsString)
import Eval2.State          (initState, RState(..))
import Eval2.Step           (rpathM)
import Zipper               (ExprZ)
import SnocList
import PrettyZ              (prettyZ)

main :: IO ()
main = do
  args  <- getArgs
  input <- case args of
    []    -> getContents
    (f:_) -> readFile f

  let prog@(Prog defMap) = fromHsString input
  -- print prog

  case Map.lookup "main" defMap of
    Nothing ->
      error "Brak definicji main"

    Just (Match _ pats rhs : _) 
      | not (null pats) ->
      error "Definicja main nie może mieć argumentów"

    Just (Match _ [] rhs : _) -> do
      -- przygotuj początkowe skupienie i stan
            -- ...existing code...
      let ez0      = (rhs, [])            :: ExprZ
          maxFuel  = 10000                :: Int
          st1 = execState
                  (modify $ \s ->
                     let h0 = history s
                     in s { history = snoc (snoc h0 (Var "main", [])) ez0 }
                  )
                  (initState defMap maxFuel)
          st2 = execState (rpathM ez0) st1
          historyList  = toList (history st2)  :: [ExprZ]
      -- ...existing code...

      -- wypisz historię redukcji z zaznaczonym kontekstem
      mapM_ (putStrLn . prettyZ) historyList 
      
      -- ewentualny komunikat o wyczerpanym paliwie
      if fuel st2 <= 0
        then putStrLn "-- Uwaga: wyczerpano paliwo, możliwe zapętlenie --"
        else return ()
        
