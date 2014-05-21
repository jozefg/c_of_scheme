module AST where
import Gen
import Error
import Control.Applicative
import Control.Monad.State
import Control.Error

data Var = SVar String | Gen Integer
         deriving(Eq, Ord, Show)

data SLit = SInt Int
          | SSym String
          deriving(Eq)
instance Show SLit where
  show (SInt i) = show i
  show (SSym s) = '\'' : s

data UserPrim = Plus | Mult | Sub | Div    | Display
              | Cons | Car  | Cdr | Eq     | Exit
              deriving(Eq, Show)

data CPSPrim = Halt | UserPrim UserPrim
              deriving(Eq, Show)

data ClosPrim = NewClos Var | SelectClos [Int] Var | MkLam
              | WriteClos | TopClos | CPSPrim CPSPrim
              deriving(Eq, Show)

data SExp p = Lit  SLit
            | Lam [Var] [SExp p]
            | Set Var (SExp p)
            | If (SExp p) (SExp p) (SExp p)
            | App (SExp p) [SExp p]
            | Var Var
            | Prim p
            deriving(Show, Eq)

data SDec p = Def Var (SExp p)
            | Init Var
            | Fun Var [Var] [SExp p]
            deriving(Eq, Show)

type Compiler = StateT Var (EitherT Failure Gen)

freshLam :: (SExp a -> Compiler (SExp a)) -> Compiler (SExp a)
freshLam f = do
  v <- Gen <$> gen
  Lam [v] . (:[]) <$> f (Var v)

