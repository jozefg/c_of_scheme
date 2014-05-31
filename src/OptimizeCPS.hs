module OptimizeCPS (optimize) where
import AST
import Data.Functor.Foldable

optimize :: Compiler (SExp CPSPrim) -> Compiler (SExp CPSPrim)
optimize = id -- Go go fast

-- | The size of term, useful for telling whether inlining/optimizing
-- actually did something useful
size :: SExp p -> Integer
size = cata folder
  where folder (LamF _ exps) = sum exps
        folder (SetF _ i)    = i + 1
        folder (IfF i t e)   = i + t + e
        folder (AppF f args) = sum (f : args)
        folder _             = 1

-- | Returns true IFF the variable is mutated within this context
mutated :: Var -> SExp p -> Bool
mutated var = cata check
  where check (SetF v b) = v == var || b
        check (LamF vars bs) = not (var `elem` vars) && or bs -- Avoid different bound variable w/ same name
        check (IfF i t e)    = i || t || e
        check (AppF f args)  = or (f : args)
        check _              = False

-- | Substitution
substitute :: Var -> SExp p -> SExp p -> SExp p
substitute var exp = para folder
  where folder (LamF vars body)       = if var `elem` vars then Lam vars (map fst body) else Lam vars (map snd body)
        folder (VarF v)               = if v == var then exp else Var v
        folder (IfF (_,i)(_,t)(_,e))  = If i t e
        folder (AppF f args)          = App (snd f) (map snd args)
        folder (LitF l)               = Lit l
        folder (SetF v e)             = Set v (snd e) -- We'll never inline a settable variable
        folder (PrimF p)              = Prim p
