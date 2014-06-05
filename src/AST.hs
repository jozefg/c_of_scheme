{-# LANGUAGE DeriveFunctor, TypeFamilies #-}
module AST where
import Utils.Gen
import Utils.Error
import Control.Applicative
import Control.Monad.State
import Control.Error
import Data.Functor.Foldable

data Var = SVar String | Gen Integer
         deriving(Eq, Ord, Show)

data SLit = SInt Int
          | SSym String
          deriving(Eq, Show)

data UserPrim = Plus | Mult | Sub | Div | Display
              | Cons | Car  | Cdr | Eq  | Exit
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


-- | Stuff for recursion-schemes to work
data SExpF p a = LitF  SLit
               | LamF [Var] [a]
               | SetF Var a
               | IfF a a a
               | AppF a [a]
               | VarF Var
               | PrimF p
               deriving Functor
type instance Base (SExp p) = SExpF p
instance Foldable (SExp p) where
  project (Lit l)         = LitF l
  project (Lam vars exps) = LamF vars exps
  project (Set v e)       = SetF v e
  project (If i t e)      = IfF i t e
  project (App f args)    = AppF f args
  project (Var v)         = VarF v
  project (Prim p)        = PrimF p

instance Unfoldable (SExp p) where
  embed (LitF l)         = Lit l
  embed (LamF vars exps) = Lam vars exps
  embed (SetF v e)       = Set v e
  embed (IfF i t e)      = If i t e
  embed (AppF f args)    = App f args
  embed (VarF v)         = Var v
  embed (PrimF p)        = Prim p 
