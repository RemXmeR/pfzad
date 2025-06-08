{-# LANGUAGE RecordWildCards #-}
module Main where

import Language.Haskell.Parser    (parseModule, ParseResult(..))
import Language.Haskell.Syntax    ( HsModule(..)
                                  , HsDecl(..), HsPat(..)
                                  , HsMatch(..), HsRhs(..), HsExp(..)
                                  , HsQName(..), HsName(..)
                                  )
import System.Environment          (getArgs)
import qualified Data.Map as Map
import Data.Map                     (Map)
import Data.List                    (foldl')
import Data.Char                    (isSpace)
import Control.Applicative          ((<|>))

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
  let name = getPatName pat in
  [ Match name [] (fromHsExp e) ]

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
fromHsExp (HsApp e1 e2) = fromHsExp e1 :$ fromHsExp e2
fromHsExp (HsParen e)   = fromHsExp e
fromHsExp e             = error ("Unsupported expression: " ++ show e)

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
buildDefMap = foldl' insertMatch Map.empty
  where
    insertMatch m mt@Match{..} =
      Map.insertWith (++) matchName [mt] m


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
      tryMatches matches args <|> reduceSubexpr
    _ -> reduceSubexpr
  where
    tryMatches []     _    = Nothing
    tryMatches (m:ms) args =
      case matchArgs (matchPats m) args of
        Just subs ->
          let n           = length (matchPats m)
              (_use, rest) = splitAt n args
              body'       = substList subs (matchRhs m)
              newExpr     = applyExpr body' rest
          in Just newExpr
        Nothing -> tryMatches ms args

    reduceSubexpr = case expr of
      e1 :$ e2 ->
        case rstep defMap e1 of
          Just e1' -> Just (e1' :$ e2)
          Nothing  ->
            case rstep defMap e2 of
              Just e2' -> Just (e1 :$ e2')
              Nothing  -> Nothing
      _ -> Nothing


-- CAŁA ŚCIEŻKA REDUKCJI (POZIOM 1) --------------------------------------------

rpath :: DefMap -> Expr -> [Expr]
rpath defMap e = e : case rstep defMap e of
                       Just e' -> rpath defMap e'
                       Nothing -> []


-- PODSTAWIANIE ---------------------------------------------------------------

subst :: (Name, Expr) -> Expr -> Expr
subst (x, s) e0 = case e0 of
  Var y     -> if x == y then s else Var y
  Con c     -> Con c
  e1 :$ e2  -> subst (x, s) e1 :$ subst (x, s) e2

-- Poprawiona definicja substList:
substList :: [(Name, Expr)] -> Expr -> Expr
substList subs expr = foldl' (\acc (x, e) -> subst (x, e) acc) expr subs


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


-- USUWANIE KOMENTARZY --------------------------------------------------------

removeBlockComments :: String -> String
removeBlockComments "" = ""
removeBlockComments s
  | "{-" `isPrefixOf` s = dropBlock s
  | otherwise           = head s : removeBlockComments (tail s)
  where
    isPrefixOf p q = take (length p) q == p
    dropBlock xs = case breakOn "-}" xs of
                     Nothing        -> ""
                     Just (_, rest) -> removeBlockComments (drop (length "-}") rest)

    breakOn :: String -> String -> Maybe (String, String)
    breakOn pat str = search "" str
      where
        search acc "" = Nothing
        search acc q@(x:xs)
          | pat `isPrefixOf` q = Just (reverse acc, drop (length pat) q)
          | otherwise          = search (x:acc) xs

removeInlineComments :: String -> String
removeInlineComments line =
  case breakOn "--" line of
    Nothing       -> line
    Just (code,_) -> code
  where
    breakOn pat str = search "" str
      where
        search acc "" = Nothing
        search acc q@(x:xs)
          | take (length pat) q == pat = Just (reverse acc, drop (length pat) q)
          | otherwise = search (x:acc) xs

prepareInput :: String -> String
prepareInput inp =
  let noBlock       = removeBlockComments inp
      noInlineLines = map removeInlineComments (lines noBlock)
      stripped      = unlines noInlineLines
      nonEmpty      = filter (not . all isSpace) (lines stripped)
      withHeader    = case nonEmpty of
                        (l:_) | "module " `isPrefixOf` l -> stripped
                        _ -> unlines ("module Temp where" : nonEmpty)
  in withHeader
  where isPrefixOf p q = take (length p) q == p


-- MAIN -----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    []    -> getContents
    (f:_) -> readFile f
  let Prog defMap = fromHsString input
  case Map.lookup "main" defMap of
    Nothing -> error "Brak definicji main"
    Just (Match _ [] rhs : _) -> do
      let path = rpath defMap rhs
      mapM_ (putStrLn . pretty) path
    _ -> error "Definicja main powinna być bez parametrów"
