{-# LANGUAGE RecordWildCards #-}

module Main where

import Language.Haskell.Parser    ( parseModule, ParseResult(..) )
import Language.Haskell.Syntax    ( HsModule(..)
                                  , HsDecl(..)
                                  , HsMatch(..)
                                  , HsRhs(..)
                                  , HsExp(..)
                                  , HsPat(..)
                                  , HsName(..)
                                  , HsQName(..)
                                  )
import Data.List                  ( groupBy, sortOn, nub )
import Data.Function              ( on )

--------------------------------------------------------------------------------
-- 1. Rozszerzenie AST
--------------------------------------------------------------------------------
type Name = String

infixl 9 :$
data Expr
    = Var Name
    | Con Name
    | Expr :$ Expr
  deriving (Eq, Show)

data Pat
    = PVar Name
    | PApp Name [Pat]
  deriving (Eq, Show)

data Match = Match
    { matchName :: Name
    , matchPats :: [Pat]
    , matchRhs  :: Expr
    }
  deriving (Eq, Show)

data Def = Def
    { defMatches :: [Match] }
  deriving (Eq, Show)

newtype Prog = Prog { progDefs :: [Def] }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- 2. Parsowanie i budowa programu
--------------------------------------------------------------------------------

fromHsString :: String -> Prog
fromHsString src =
  case parseModule src of
    ParseOk     hsMod   -> Prog (fromHsModule hsMod)
    ParseFailed loc err -> error $ "Parse error at " ++ show loc ++ ": " ++ err

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ decls) =
  let allMatches :: [Match]
      allMatches = concatMap fromHsDecl decls

      grouped :: [[Match]]
      grouped = groupBy ((==) `on` matchName) (sortOn matchName allMatches)

      defs :: [Def]
      defs = [ Def ms | ms@(m:_) <- grouped ]
  in defs

fromHsDecl :: HsDecl -> [Match]
-- Obsługa prostego wiązania wzorcowego: x = expr
fromHsDecl (HsPatBind _ pat rhs _) =
  case pat of
    HsPVar hsName ->
      let name    = identToString hsName
          rhsExpr = case rhs of
                      HsUnGuardedRhs e -> fromHsExp e
                      HsGuardedRhss _  -> error "Guarded expressions not supported"
      in [Match name [] rhsExpr]
    _ -> error $ "Unsupported pattern-binding (only variable = expr): " ++ show pat

-- Obsługa definicji funkcyjnej z wzorcami
fromHsDecl (HsFunBind matches) = map fromHsMatch matches

fromHsDecl decl =
    error $ "Unsupported declaration (only function-bindings or simple pat-binds allowed): " ++ show decl

fromHsMatch :: HsMatch -> Match
fromHsMatch (HsMatch _ hsName hsPats hsRhs _) =
    let name    = identToString hsName
        pats    = map fromHsPat hsPats
        rhsExpr = case hsRhs of
                    HsUnGuardedRhs e -> fromHsExp e
                    HsGuardedRhss _  -> error "Guarded expressions not supported"
    in Match name pats rhsExpr

fromHsPat :: HsPat -> Pat
fromHsPat (HsPVar hsName) =
    PVar (identToString hsName)
fromHsPat (HsPApp hsCon hsPats) =
    let conName = qNameToString hsCon
        pats    = map fromHsPat hsPats
    in PApp conName pats
fromHsPat (HsPParen p) =
    fromHsPat p
fromHsPat p =
    error $ "Unsupported pattern (only variable or constructor patterns, possibly in parentheses): " ++ show p

fromHsExp :: HsExp -> Expr
fromHsExp (HsVar qn)    = Var  (qNameToString qn)
fromHsExp (HsCon qn)    = Con  (qNameToString qn)
fromHsExp (HsApp e1 e2) = fromHsExp e1 :$ fromHsExp e2
fromHsExp (HsParen e)   = fromHsExp e
fromHsExp e             = error $ "Unsupported expression: " ++ show e

qNameToString :: HsQName -> String
qNameToString (UnQual hsName) = identToString hsName
qNameToString (Qual _ hsName) = identToString hsName
qNameToString (Special s)     = error $ "Unsupported special name: " ++ show s

identToString :: HsName -> String
identToString (HsIdent  s) = s
identToString (HsSymbol s) = s

--------------------------------------------------------------------------------
-- 3. Weryfikacja poprawności programu
--------------------------------------------------------------------------------

-- Znajduje duplikujące się elementy w liście (zwraca pierwszą napotkaną duplikację)
findDuplicate :: Eq a => [a] -> Maybe a
findDuplicate []     = Nothing
findDuplicate (x:xs) = if x `elem` xs then Just x else findDuplicate xs

-- Sprawdza, czy w pojedynczym wzorcu nie ma powtórzonych zmiennych
uniqueVarsInPat :: Pat -> Bool
uniqueVarsInPat pat = case pat of
  PVar x      -> True
  PApp _ ps   -> let vars = concatMap collectVars ps
                 in length vars == length (nub vars)
  where
    collectVars :: Pat -> [Name]
    collectVars (PVar x)    = [x]
    collectVars (PApp _ ps) = concatMap collectVars ps

-- Sprawdza poprawność jednego Match: wzorce muszą mieć unikalne zmienne
checkMatch :: Match -> Either String ()
checkMatch (Match name pats _) =
  case [ p | p <- pats, not (uniqueVarsInPat p) ] of
    (bad:_) -> Left $ "Wzorzec w definicji '" ++ name ++ "' zawiera powtórzone zmienne: " ++ show bad
    []      -> Right ()

-- Sprawdza poprawność całego Def: wszystkie Matches mają tę samą nazwę i poprawne wzorce
checkDef :: Def -> Either String ()
checkDef (Def matches) =
  let names = map matchName matches
  in case findDuplicate names of
       Just dup -> Left $ "W jednych Def są dwa Matches o tej samej nazwie: " ++ dup
       Nothing  -> mapM_ checkMatch matches

-- Sprawdza, czy istnieje dokładnie jeden Match 'main' bez wzorców
checkMain :: [Def] -> Either String ()
checkMain defs =
  let mains = [ m | Def ms <- defs, m <- ms, matchName m == "main" ]
  in case mains of
       [] -> Left "Brak definicji 'main'"
       xs -> case filter (\m -> null (matchPats m)) xs of
               []  -> Left "Nie znaleziono 'main' bez wzorców (main nie przyjmuje argumentów)"
               [_] -> Right ()
               _   -> Left "Znalazłem więcej niż jedną definicję 'main' bez wzorców"

-- Główna funkcja weryfikująca program
checkProg :: Prog -> Either String Prog
checkProg prog@(Prog defs) = do
  -- Sprawdź, że nie ma dwóch Def o tej samej nazwie głównej (jednak każdy Def grupuje Matches tej samej nazwy
  -- więc już grupowanie zapewnia unikalność nazw Def; tu dodatkowa kontrola nie jest konieczna)

  -- Sprawdź każdy Def
  mapM_ checkDef defs

  -- Sprawdź definicję main
  checkMain defs

  return prog

--------------------------------------------------------------------------------
-- 4. Prosty test w main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let testSrc = unlines
        [ "one = S Z"
        , "two = S one"
        , "add Z     n = n"
        , "add (S m) n = S (add m n)"
        , "main = add two two"
        ]
  putStrLn "Źródło programu do sparsowania:"
  putStrLn testSrc

  putStrLn "\nParsowanie na AST (Prog):"
  let prog = fromHsString testSrc
  print prog

  putStrLn "\nWeryfikacja poprawności programu:"
  case checkProg prog of
    Left err -> putStrLn $ "Błąd: " ++ err
    Right _  -> putStrLn "Program poprawny"

  putStrLn "\nSzczegóły definicji (każda Def i jej Match-e):"
  mapM_ printDef (progDefs prog)

printDef :: Def -> IO ()
printDef (Def matches) = do
  putStrLn "Definicja funkcji:"
  mapM_ printMatch matches
  putStrLn ""

printMatch :: Match -> IO ()
printMatch (Match name pats rhs) = do
  putStr $ name ++ concatMap (" " ++) (map showPat pats) ++ " = "
  putStrLn (showExpr rhs)

showPat :: Pat -> String
showPat (PVar x)    = x
showPat (PApp c ps) = "(" ++ c ++ " " ++ unwords (map showPat ps) ++ ")"

showExpr :: Expr -> String
showExpr (Var x)    = x
showExpr (Con c)    = c
showExpr (e1 :$ e2) = "(" ++ showExpr e1 ++ " " ++ showExpr e2 ++ ")"
