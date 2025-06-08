{-# LANGUAGE RecordWildCards #-}
module Main where

import Language.Haskell.Parser    (parseModule, ParseResult(..))
import Language.Haskell.Syntax    ( HsModule(..)
                                  , HsDecl(..), HsPat(..)
                                  , HsMatch(..), HsRhs(..), HsExp(..)
                                  , HsQName(..), HsName(..)
                                  , HsLiteral(..)
                                  )
import System.Environment          (getArgs)
import qualified Data.Map as Map
import Data.Map                     (Map)
import Data.List (foldl', partition)
import Data.Char                    (isSpace)
import Control.Applicative          ((<|>))
import Data.Maybe (maybe)

-- AST ------------------------------------------------------------------------

type Name = String

data Pat
  = PVar Name
  | PApp Name [Pat]
  deriving (Eq, Show)

data Match = Match
  { matchName :: Name
  , matchPats :: [Pat]
  , matchRhs  :: Expr
  } deriving (Eq, Show)

data Expr
  = Var Name
  | Con Name
  | Expr :$ Expr
  deriving (Eq, Show)
infixl 9 :$

type DefMap = Map Name [Match]

newtype Prog = Prog { progDefs :: DefMap }
  deriving (Eq, Show)


-- PARSER (używając haskell-src) ------------------------------------------------

fromHsString :: String -> Prog
fromHsString s =
  case parseModule (prepareInput s) of
    ParseOk     hsMod    -> Prog (buildDefMap (fromHsModule hsMod))
    ParseFailed loc err  -> error ("Parse error at " ++ show loc ++ ": " ++ err)

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


-- SPŁASZCZENIE APLIKACJI --------------------------------------------------------

flattenApp :: Expr -> (Expr, [Expr])
flattenApp (e :$ e') =
  let (f, args) = flattenApp e
  in (f, args ++ [e'])
flattenApp e = (e, [])

applyExpr :: Expr -> [Expr] -> Expr
applyExpr = foldl' (:$)


-- PROSTE DOPASOWANIE WZORCA (POZIOM 1) -----------------------------------------

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

-- PRETTY PRINTING ------------------------------------------------------------

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


-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    []    -> getContents
    (f:_) -> readFile f
  let prog@(Prog defMap) = fromHsString input
  
  case Map.lookup "main" defMap of
    Nothing -> error "Brak definicji main"
    Just (Match _ [] rhs : _) -> do
      let path = rpath defMap rhs
      mapM_ (putStrLn . pretty) path
    _ -> error "Definicja main powinna być bez parametrów"
  -- print prog
  -- print defMap
  -- mapM_ print (Map.toList defMap)

