module Pretty
  ( pretty
  ) where

import AST
-- Chce importować tylko flatten z Eval1
import Eval1 (flattenApp)

-- tylko funkcja pretty i atom, importuje flattenApp z Eval1 lub AST
pretty :: Expr -> String
pretty e = case flattenApp e of
  (Var n, []) -> n
  (Con n, []) -> n
  (f, args)   -> unwords (map atom (f:args))
  where
    atom (Var x) = x
    atom (Con c) = c
    atom e'      = "(" ++ pretty e' ++ ")"


prepareInput :: String -> String
prepareInput = id