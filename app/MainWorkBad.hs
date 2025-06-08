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
import Control.Applicative          (Alternative(..), (<|>))
import Control.Monad.State.Strict
import Data.Maybe                   (isJust)

--------------------------------------------------------------------------------
-- Poziom 2: pełne śledzenie redukcji z kontekstem ("zipper" z Top) i "fuel"
--------------------------------------------------------------------------------

-- 1. Definicje AST i wzorców -------------------------------------------------

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

infixl 9 :$
data Expr
  = Var Name
  | Con Name
  | Expr :$ Expr
  deriving (Eq, Show)

type DefMap = Map Name [Match]

newtype Prog = Prog { progDefs :: DefMap }
  deriving (Eq, Show)


-- 2. Parser (haskell-src) ----------------------------------------------------

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
        , matchRhs  = fromHsExp (case rhs of
                                   HsUnGuardedRhs e -> e
                                   _                -> error "Guards not supported")
        }

fromHsPat :: HsPat -> Pat
fromHsPat (HsPVar ident)      = PVar  (identToString ident)
fromHsPat (HsPApp qn pats)    = PApp  (qNameToString qn) (map fromHsPat pats)
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


-- 3. Budowanie mapy definicji ------------------------------------------------

buildDefMap :: [Match] -> DefMap
buildDefMap = foldl' insertMatch Map.empty
  where
    insertMatch m mt@Match{..} =
      Map.insertWith (++) matchName [mt] m


-- 4. Podstawianie ------------------------------------------------------------

subst :: (Name, Expr) -> Expr -> Expr
subst (x, s) e0 = case e0 of
  Var y     -> if x == y then s else Var y
  Con c     -> Con c
  e1 :$ e2  -> subst (x, s) e1 :$ subst (x, s) e2

substList :: [(Name, Expr)] -> Expr -> Expr
substList subs expr = foldl' (\acc (x, e) -> subst (x, e) acc) expr subs


-- 5. Spłaszczanie / składanie aplikacji -------------------------------------

flattenApp :: Expr -> (Expr, [Expr])
flattenApp (e :$ e') =
  let (f, args) = flattenApp e
  in (f, args ++ [e'])
flattenApp e = (e, [])

applyExpr :: Expr -> [Expr] -> Expr
applyExpr = foldl' (:$)


-- 6. Zipper dla Expr (z Top) ------------------------------------------------

data Ctxt
  = Top
  | LeftCrumb  Expr Ctxt   -- fokus to lewa część, prawym dzieckiem jest Expr
  | RightCrumb Expr Ctxt   -- fokus to prawa część, lewym dzieckiem jest Expr
  deriving (Eq, Show)

type ExprZ = (Expr, Ctxt)

-- Przejście w dół do lewego podwyrażenia:
goDownLeft :: ExprZ -> Maybe ExprZ
goDownLeft (e1 :$ e2, ctxt) = Just (e1, LeftCrumb e2 ctxt)
goDownLeft _                = Nothing

-- Przejście w dół do prawego podwyrażenia:
goDownRight :: ExprZ -> Maybe ExprZ
goDownRight (e1 :$ e2, ctxt) = Just (e2, RightCrumb e1 ctxt)
goDownRight _                = Nothing

-- Powrót w górę do rodzica:
goUp :: ExprZ -> Maybe ExprZ
goUp (f, LeftCrumb r ctxt)  = Just (f :$ r, ctxt)
goUp (f, RightCrumb l ctxt) = Just (l :$ f, ctxt)
goUp (_, Top)               = Nothing

-- Przechodzimy do korzenia:
toRoot :: ExprZ -> ExprZ
toRoot z@(_, Top) = z
toRoot z          = case goUp z of
                      Just parent -> toRoot parent
                      Nothing     -> z


-- 7. Sprawdzenie prawdziwego redexu (pattern rzeczywiście pasuje) ------------

isRedex :: DefMap -> Expr -> Bool
isRedex defMap expr =
  case flattenApp expr of
    (Var f, args)
      | Just ms <- Map.lookup f defMap ->
          any (\m -> isJust (matchArgs (matchPats m) args)) ms
    _ -> False


-- 8. Znalezienie lewostronno-zewnętrznego redexu -----------------------------

findRedex :: DefMap -> ExprZ -> Maybe ExprZ
findRedex defMap z@(e, ctxt)
  | isRedex defMap e = Just z
  | otherwise = case e of
      _ :$ _ ->
        case goDownLeft z of
          Just zL -> case findRedex defMap zL of
                       Just z' -> Just z'
                       Nothing -> case goDownRight z of
                                    Just zR -> findRedex defMap zR
                                    Nothing -> Nothing
          Nothing -> Nothing
      _ -> Nothing


-- 9. Dopasowanie wzorca (jak w poziomie 1) ------------------------------------

matchPat :: Pat -> Expr -> Maybe [(Name, Expr)]
matchPat (PVar x)    e = Just [(x, e)]
matchPat (PApp c ps) e =
  case flattenApp e of
    (Con c', es)
      | c == c' && length ps == length es ->
          fmap concat . sequence $ zipWith matchPat ps es
    _ -> Nothing

matchArgs :: [Pat] -> [Expr] -> Maybe [(Name, Expr)]
matchArgs []     _  = Just []
matchArgs (_:_)  [] = Nothing
matchArgs ps     es
  | length ps > length es = Nothing
  | otherwise =
    let (as, _) = splitAt (length ps) es
    in fmap concat . sequence $ zipWith matchPat ps as


-- Pomocnicza funkcja: znajduje pierwszy pasujący match i subs:
findMatch :: [Match] -> [Expr] -> Maybe (Match, [(Name, Expr)])
findMatch []     _    = Nothing
findMatch (m:ms) args =
  case matchArgs (matchPats m) args of
    Just subs -> Just (m, subs)
    Nothing   -> findMatch ms args


-- 10. Redukcja w danym lokalnym fokusie (przy założeniu, że to redex) -------

reduceAtFocus :: DefMap -> ExprZ -> Expr
reduceAtFocus defMap (e, _) =
  case flattenApp e of
    (Var f, args)
      | Just ms <- Map.lookup f defMap
      , Just (m, subs) <- findMatch ms args ->
        let pats         = matchPats m
            n            = length pats
            (_used, rest) = splitAt n args
            body'        = substList subs (matchRhs m)
        in applyExpr body' rest
    _ -> e  -- (tego nie powinno się wywołać, jeśli isRedex==True)


-- 11. Przejście do określonego argumentu w redexie --------------------------

gotoArg :: ExprZ -> Int -> ExprZ
gotoArg redexZ i =
  let (e, ctxt) = redexZ
      (_, args) = flattenApp e
      numArgs   = length args
      -- jeśli mamy n argumentów, to żeby dojść do args[i], wykonujemy:
      -- najpierw (numArgs - i - 1) razy goDownLeft, potem raz goDownRight
      descendLeft 0 z = z
      descendLeft k z = case goDownLeft z of
                          Just z' -> descendLeft (k - 1) z'
                          Nothing -> z
      descendRight z = case goDownRight z of
                         Just z' -> z'
                         Nothing -> z
  in descendRight (descendLeft (numArgs - i - 1) redexZ)


-- 12. SnocList i jego instancje ------------------------------------------------

newtype SnocList a = SnocList { unSnocList :: [a] }
  deriving (Eq, Show)

toList   :: SnocList a -> [a]
toList = unSnocList

fromList :: [a] -> SnocList a
fromList = SnocList

snoc     :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList (xs ++ [x])

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList (xs ++ ys)

instance Monoid (SnocList a) where
  mempty = SnocList []
  mappend = (<>)

instance Functor SnocList where
  fmap f (SnocList xs) = SnocList (map f xs)

instance Applicative SnocList where
  pure x = SnocList [x]
  SnocList fs <*> SnocList xs = SnocList [ f x | f <- fs, x <- xs ]

instance Alternative SnocList where
  empty = SnocList []
  SnocList xs <|> SnocList ys = SnocList (xs ++ ys)


-- 13. Stan redukcji ------------------------------------------------------------

data RState = RState
  { defs    :: DefMap
  , fuel    :: Int
  , history :: SnocList ExprZ
  }

type R a = State RState a


-- 14. Dodaj ramkę do historii --------------------------------------------------

logFrame :: ExprZ -> R ()
logFrame z = modify $ \s -> s { history = snoc (history s) z }


-- 15. Pojedynczy krok redukcji (z użyciem zipper i DefMap) ----------------------

stepOnce :: ExprZ -> R (Maybe ExprZ)
stepOnce rootZ = do
  RState{..} <- get
  if fuel <= 0
    then return Nothing
    else case findRedex defs rootZ of
      Nothing -> return Nothing
      Just redexZ@(e, ctxt) ->
        let (Var f, args) = flattenApp e
            Just ms       = Map.lookup f defs
            Just (m, subs) = findMatch ms args
            -- znajdujemy indeks pierwszego PApp w matchPats m
            patIdx = case [ idx | (idx, pat) <- zip [0..] (matchPats m), case pat of PApp{} -> True; _ -> False ] of
                       (i:_) -> i
                       []    -> -1
        in do
          -- 1) jeśli patIdx >= 0, to logujemy najpierw fokus na tym argumencie
          when (patIdx >= 0) $ do
            let argZ = gotoArg redexZ patIdx
            logFrame argZ

          -- 2) teraz faktyczna redukcja redexu
          let reduced = reduceAtFocus defs redexZ
              newZ    = toRoot (reduced, ctxt)
          modify $ \s -> s { fuel = fuel - 1 }
          return (Just newZ)


-- 16. Pełna ścieżka redukcji w monadzie (z zapisem historii) -------------------

reduceFull :: ExprZ -> R ExprZ
reduceFull z0 = do
  logFrame z0
  mNext <- stepOnce z0
  case mNext of
    Nothing    -> return z0
    Just zNext -> reduceFull zNext


-- 17. Pretty-printing z wyróżnieniem fokusu -----------------------------------

-- highlight: obkłada fokus w “{…}” i dodaje nawiasy w górę kontekstu
highlight :: Expr -> Ctxt -> String
highlight focus Top = "{" ++ pretty focus ++ "}"
highlight focus (LeftCrumb r rest)  =
  let leftStr  = highlight focus rest
      rightStr = pretty r
  in              "(" ++ leftStr ++ " " ++ rightStr ++ ")"
highlight focus (RightCrumb l rest) =
  let leftStr  = pretty l
      rightStr = highlight focus rest
  in              "(" ++ leftStr ++ " " ++ rightStr ++ ")"

removeOutParens :: String -> String
removeOutParens s
  | head s == '(' && last s == ')' = init (tail s)
  | otherwise                       = s

prettyZipper :: ExprZ -> String
prettyZipper (e, ctxt) = removeOutParens (highlight e ctxt)


-- 18. Pretty dla "czystego" Expr ---------------------------------------------

pretty :: Expr -> String
pretty e = case flattenApp e of
  (Var n, []) -> n
  (Con n, []) -> n
  (f, args)   -> unwords (map atom (f:args))
  where
    atom (Var x) = x
    atom (Con c) = c
    atom e'      = "(" ++ pretty e' ++ ")"


-- 19. Usuwanie komentarzy -----------------------------------------------------

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


-- 20. Główna funkcja ----------------------------------------------------------

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
      -- 1) wypisz "{main}"
      let initialFrame = (Var "main", Top)
      -- 2) redukuj ciało: "{add two two}", potem fokus na argumencie, itd.
      let rootZ0 = (rhs, Top)

      let initState = RState
            { defs    = defMap
            , fuel    = 10000            -- limit kroków
            , history = SnocList []
            }

      -- najpierw zaloguj "{main}"
      let stateWithMain = execState (logFrame initialFrame) initState
      -- potem redukuj body
      let (_, finalSt)   = runState (reduceFull rootZ0) stateWithMain
      let frames         = toList (history finalSt)
      mapM_ (putStrLn . prettyZipper) frames

    _ -> error "Definicja main powinna być bez parametrów"
