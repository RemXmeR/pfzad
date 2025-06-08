module PrettyZ
  ( prettyZ
  ) where

import AST        (Expr)
import Zipper     (ExprZ, Context(..))
import Pretty      (pretty)  -- standardowe pretty z poziomu 1

-- | Wstawia nawiasy „{” i „}” wokół fokusu, a potem odtwarza całe wyrażenie,
--   odbudowując aplikację zgodnie z listą kontekstów.
prettyZ :: ExprZ -> String
prettyZ (focus, ctx) =
  let
    -- krok 1: string fokusu
    focusStr = "{" ++ pretty focus ++ "}"

    -- krok 2: odbuduj aplikację w górę drzewa
    build :: String -> [Context] -> String
    build s []               = s
    build s (c:cs) = case c of
      InFunc arg ->
        -- fokus jest funkcją: ( fokus arg )
        build ("(" ++ s ++ " " ++ pretty arg ++ ")") cs
      InArg fun  ->
        -- fokus jest argumentem: ( fun fokus )
        build ("(" ++ pretty fun ++ " " ++ s ++ ")") cs

    raw = build focusStr ctx

    -- usuń zewnętrzne nawiasy, jeśli występują
    trimParens str
      | head str == '(' && last str == ')' = init (tail str)
      | otherwise                          = str
  in
    trimParens raw
