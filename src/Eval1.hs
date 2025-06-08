module Eval1
  ( flattenApp, rstep, rpath, matchArgs
  ) where

import AST
import Data.List      (foldl', partition)
import qualified Data.Map as Map
import Control.Applicative ((<|>))

-- wszystkie funkcje:
-- flattenApp, applyExpr
-- matchPat, matchArgs
-- subst, substList
-- rstep (pełna wersja poziom 1)
-- rpath

-- SPŁASZCZENIE APLIKACJI --------------------------------------------------------


flattenApp :: Expr -> (Expr, [Expr])
flattenApp (e :$ e') =
  let (f, args) = flattenApp e
  in (f, args ++ [e'])
flattenApp e = (e, [])


applyExpr :: Expr -> [Expr] -> Expr
applyExpr = foldl' (:$)



matchPat :: Pat -> Expr -> Maybe [(Name, Expr)]
matchPat (PVar x)    e = Just [(x, e)]
matchPat (PApp c ps) e =
  case flattenApp e of
    (Con c', es) | c == c' && length ps == length es ->
      fmap concat . sequence $ zipWith matchPat ps es
    _ -> Nothing

matchArgs :: [Pat] -> [Expr] -> Maybe [(Name, Expr)]
matchArgs []     _        = Just []
matchArgs (_:_)  []       = Nothing
matchArgs ps     es
  | length ps > length es = Nothing
  | otherwise =
    let (as, _) = splitAt (length ps) es
    in fmap concat . sequence $ zipWith matchPat ps as


-- POJEDYNCZY KROK REDUKCJI (POZIOM 1) ------------------------------------------
rstep :: DefMap -> Expr -> Maybe Expr
rstep defMap expr =
  case flattenApp expr of
    (Var f, args) | Just matches <- Map.lookup f defMap ->
      let
        -- zachowujemy kolejność z pliku:
        --  konkretne wzorce (z PApp) najpierw,
        --  a potem catch-all (tylko PVar)
        specific = filter (any (not . isPVar) . matchPats) matches
        catchAll = filter (all isPVar . matchPats) matches
      in
        if not (null specific)
          then
            -- najpierw konkretne wzorce,
            -- jeśli nic nie zadziała, redukcja pierwszego dopasowywalnego argumentu,
            -- na końcu klauzule catch-all
            tryMatches specific args
            <|> reduceFirstArg f args
            <|> tryMatches catchAll args
          else
            -- brak żadnych konkretnych wzorców → od razu catch-all
            tryMatches catchAll args
            <|> reduceSubexpr expr

    _ ->
      -- w pozostałych miejscach standardowa rekursja lewo-prawo
      reduceSubexpr expr

 where
  isPVar :: Pat -> Bool
  isPVar (PVar _) = True
  isPVar _        = False

  tryMatches :: [Match] -> [Expr] -> Maybe Expr
  tryMatches [] _ = Nothing
  tryMatches (m:ms) args =
    case matchArgs (matchPats m) args of
      Just subs ->
        let n       = length (matchPats m)
            (_, rs) = splitAt n args
            body'   = substList subs (matchRhs m)
        in  Just (applyExpr body' rs)
      Nothing ->
        tryMatches ms args

  reduceFirstArg :: Name -> [Expr] -> Maybe Expr
  reduceFirstArg f args =
    foldl' (<|>) Nothing
      [ case splitAt i args of
          (bs, a:as) ->
            fmap (\a' -> applyExpr (Var f) (bs ++ a' : as))
                 (rstep defMap a)
          _ -> Nothing
      | i <- [0 .. length args - 1]
      ]

  reduceSubexpr :: Expr -> Maybe Expr
  reduceSubexpr (e1 :$ e2) =
       (rstep defMap e1 >>= \e1' -> Just (e1' :$ e2))
   <|> (rstep defMap e2 >>= \e2' -> Just (e1 :$ e2'))
  reduceSubexpr _ = Nothing






-- rstep :: DefMap -> Expr -> Maybe Expr
-- rstep defMap expr
--   -- 1) Główna aplikacja: Var f args, gdzie f w defMap
--   | (Var f, args) <- flattenApp expr, 
--   Just matches <- Map.lookup f defMap
--   = tryMatches matches args
--     <|> reduceSubexpr expr

--   -- 2) Inaczej: schodzimy w podwyrażenia
--   | otherwise
--   = reduceSubexpr expr

--  where
--   -- próbuj wszystkie reguły jedna po drugiej
--   tryMatches :: [Match] -> [Expr] -> Maybe Expr
--   tryMatches [] _ = Nothing
--   tryMatches (m:ms) args =
--     ( matchArgs (matchPats m) args >>= \subs ->
--         let n         = length (matchPats m)
--             (_, rest) = splitAt n args
--             body'     = substList subs (matchRhs m)
--             newE      = applyExpr body' rest
--         in Just newE
--     )
--     <|> tryMatches ms args

--   -- redukcja rekurencyjna w podwyrażeniach
--   reduceSubexpr :: Expr -> Maybe Expr
--   reduceSubexpr e@(e1 :$ e2) =
--        ( rstep defMap e1 >>= \e1' -> Just (e1' :$ e2) )
--    <|> ( rstep defMap e2 >>= \e2' -> Just (e1 :$ e2') )
--   reduceSubexpr _ = Nothing


-- CAŁA ŚCIEŻKA REDUKCJI (POZIOM 1) --------------------------------------------

rpath :: DefMap -> Expr -> [Expr]
rpath defMap e = e : case rstep defMap e of
                       Just e' -> rpath defMap e'
                       Nothing -> []


-- rpath :: DefMap -> Expr -> [Expr]
-- rpath defMap e =
--   e : maybe [] (rpath defMap) (rstep defMap e)



-- PODSTAWIANIE ---------------------------------------------------------------

subst :: (Name, Expr) -> Expr -> Expr
subst (x, s) e0 = case e0 of
  Var y     -> if x == y then s else Var y
  Con c     -> Con c
  e1 :$ e2  -> subst (x, s) e1 :$ subst (x, s) e2

-- Poprawiona definicja substList:
-- substList :: [(Name, Expr)] -> Expr -> Expr
-- substList subs expr = foldl' (\acc (x, e) -> subst (x, e) acc) expr subs


substList :: [(Name, Expr)] -> Expr -> Expr
substList subs expr = go expr
  where
    subMap = Map.fromList subs
    go (Var y)     = Map.findWithDefault (Var y) y subMap
    go (Con c)     = Con c
    go (e1 :$ e2)  = go e1 :$ go e2
