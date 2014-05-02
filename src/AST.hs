module AST where
import Gen
import Error
import Control.Applicative
import Control.Monad.State
import Control.Error

data Var = SVar String | Gen Integer
         deriving(Eq, Ord)
instance Show Var where
  show (SVar v) = v
  show (Gen  i) = "%gen_"++show i

data SLit = SInt Int
          | SSym String
          deriving(Eq)
instance Show SLit where
  show (SInt i) = show i
  show (SSym s) = '\'' : s

data UserPrim = Plus | Mult | Sub | Div    | Display
              | Cons | Car  | Cdr | CallCC | Eq | Exit
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
            deriving(Eq)
instance Show p => Show (SExp p) where
  show (Lit l) = show l
  show (Lam vars exps) =
    "(lambda ("++ unwords (map show vars) ++ ")" ++
    '\n' : unwords (map show exps) ++ ")"
  show (Set v e) = "( set! " ++ show v ++ " " ++ show e ++ ")"
  show (If test true false) =
    "(if " ++ show test ++ "\n" ++ show true ++ "\n" ++ show false ++ ")"
  show (App f args) = "(" ++ show f ++ " " ++ unwords (map show args) ++ ")"
  show (Var v) = show v
  show (Prim p) = show p
  
data SDec p = Def Var (SExp p)
            | Init Var
            | Fun Var [Var] [SExp p]
            deriving(Eq)

instance Show p => Show (SDec p) where
  show (Def v e) = "(define "++show v++"\n  "++show e++")"

type Compiler = StateT Var (EitherT Failure Gen)

freshLam :: (SExp a -> Compiler (SExp a)) -> Compiler (SExp a)
freshLam f = do
  v <- Gen <$> gen
  Lam [v] . (:[]) <$> f (Var v)

