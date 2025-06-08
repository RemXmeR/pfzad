module AST (module AST) where
-- ...reszta kodu...

import qualified Data.Map as Map
import Data.Map (Map)

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
