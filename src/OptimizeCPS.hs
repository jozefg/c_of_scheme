module OptimizeCPS (optimize) where
import AST
import Data.Functor.Foldable

optimize :: Compiler (SExp CPSPrim) -> Compiler (SExp CPSPrim)
optimize = fmap inline

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
        check (LamF vars bs) = notElem var vars && or bs -- Avoid different bound variable w/ same name
        check (IfF i t e)    = i || t || e
        check (AppF f args)  = or (f : args)
        check _              = False

-- | Substitution
substitute :: Var -> SExp p -> SExp p -> SExp p
substitute var exp = para folder
  where folder (LamF vars body)       = Lam vars $ if var `elem` vars then map fst body else map snd body
        folder (VarF v)               = if v == var then exp else Var v
        folder (IfF (_,i)(_,t)(_,e))  = If i t e
        folder (AppF f args)          = App (snd f) (map snd args)
        folder (LitF l)               = Lit l
        folder (SetF v e)             = Set v (snd e) -- We'll never inline a settable variable
        folder (PrimF p)              = Prim p

effectFree :: SExp p -> Bool
effectFree = cata pureish
  where pureish VarF{} = True
        pureish LitF{} = True
        pureish _      = False

inline :: SExp p -> SExp p
inline = cata folder
  where folder (AppF (Lam [v] [e]) [a]) -- Simple inlining, inline only small pure arguments
          | mutated v e && size a < 10 && effectFree a = substitute v a e 
        folder e = embed e
