{-# LANGUAGE RecordWildCards #-}
module Parser
  ( fromHsString
  ) where

import AST
import Language.Haskell.Parser    (parseModule, ParseResult(..))
import Language.Haskell.Syntax    ( HsModule(..), HsDecl(..), HsPat(..)
                                  , HsMatch(..), HsRhs(..), HsExp(..)
                                  , HsQName(..), HsName(..), HsLiteral(..)
                                  )
import Data.List           (foldl')
import qualified Data.Map as Map

fromHsString :: String -> Prog
fromHsString s =
  case parseModule (prepareInput s) of
    ParseOk     hsMod   -> Prog (buildDefMap (fromHsModule hsMod))
    ParseFailed loc err -> error ("Parse error at " ++ show loc ++ ": " ++ err)

-- tutaj z fromHsModule, fromHsDecl, fromHsMatch, fromHsPat, fromHsExp,
-- literalToString, qNameToString, identToString, getPatName, buildDefMap

-- PARSER ------------------------------------------------

fromHsModule :: HsModule -> [Match]
fromHsModule (HsModule _ _ _ _ decls) =
  concatMap fromHsDecl decls

fromHsDecl :: HsDecl -> [Match]
fromHsDecl (HsFunBind matches) =
  map fromHsMatch matches

fromHsDecl (HsPatBind _ pat (HsUnGuardedRhs e) _) =
  let
    (name, pats) = case pat of
      HsPVar ident ->
        (identToString ident, [])

      HsPApp qn hsPats ->
        ( qNameToString qn
        , map fromHsPat hsPats
        )

      HsPParen p ->
        case p of
          HsPVar ident ->
            (identToString ident, [])
          HsPApp qn hsPats ->
            ( qNameToString qn
            , map fromHsPat hsPats
            )
          _ ->
            error ("Unsupported pattern-bind: " ++ show pat)

      _ ->
        error ("Unsupported pattern-bind: " ++ show pat)
  in
    [ Match name pats (fromHsExp e) ]

fromHsDecl d =
  error ("Unsupported declaration: " ++ show d)

fromHsMatch :: HsMatch -> Match
fromHsMatch (HsMatch _ name pats rhs _) =
  Match { matchName = identToString name
        , matchPats = map fromHsPat pats
        , matchRhs  = fromHsExp (case rhs of HsUnGuardedRhs e -> e; _ -> error "Guards not supported")
        }

-- TERAZ obsługujemy też HsPParen:
fromHsPat :: HsPat -> Pat
fromHsPat (HsPVar ident)      = PVar (identToString ident)
fromHsPat (HsPApp qn pats)    = PApp (qNameToString qn) (map fromHsPat pats)
fromHsPat (HsPParen p)        = fromHsPat p
fromHsPat p                   = error ("Unsupported pattern: " ++ show p)

fromHsExp :: HsExp -> Expr
fromHsExp (HsVar qn)    = Var (qNameToString qn)
fromHsExp (HsCon qn)    = Con (qNameToString qn)
fromHsExp (HsLit lit)   = Con (literalToString lit)
fromHsExp (HsApp e1 e2) = fromHsExp e1 :$ fromHsExp e2
fromHsExp (HsParen e)   = fromHsExp e
fromHsExp e             = error ("Unsupported expression: " ++ show e)

literalToString :: HsLiteral -> Name
literalToString (HsInt i)      = show i
literalToString (HsChar c)     = show c
literalToString (HsString s)   = show s
literalToString (HsFrac r)     = show r
literalToString other          = error ("Unsupported literal: " ++ show other)

qNameToString :: HsQName -> String
qNameToString (UnQual ident) = identToString ident
qNameToString (Qual _ ident) = identToString ident
qNameToString (Special _)    = error "Special names not supported"

identToString :: HsName -> String
identToString (HsIdent  s) = s
identToString (HsSymbol s) = s

getPatName :: HsPat -> Name
getPatName (HsPVar ident) = identToString ident
getPatName p              = error ("Pattern-bind: unsupported " ++ show p)



-- BUDOWANIE MAPY DEFINICJI ----------------------------------------------------

buildDefMap :: [Match] -> DefMap
-- foldl' to od razu oblicza nową wartość, zamiast tworzyć zalegające thunki. 
-- Dzięki temu unika przepełnienia stosu i nadmiernego zużycia pamięci.
-- np. foldl (+) 0 [1,2,3,4] 
  -- krok 1: ((0 + 1) + 2) + 3) + 4 == 10

buildDefMap = foldl' insertMatch Map.empty
  where
    insertMatch m mt@Match{..} =
      Map.insertWith (flip (++)) matchName [mt] m


prepareInput :: String -> String
prepareInput = id