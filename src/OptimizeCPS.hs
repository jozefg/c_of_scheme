module OptimizeCPS (optimizeCPS) where
import Data.Functor.Foldable
import AST
import Utils.Error

optimizeCPS :: [SDec CPSPrim] -> Compiler [SDec CPSPrim]
optimizeCPS decs = mapM optimizer decs
  where optimizer Def{}         = failCPS "optimizeCPS" "Unexpected Def"
        optimizer (Fun v vs es) = Fun v vs `fmap` mapM (optimizeExp decs) es
        optimizer (Init n)      = return $ Init n


optimizeExp :: [SDec CPSPrim] -> SExp CPSPrim -> Compiler (SExp CPSPrim)
optimizeExp decs = return . inlineExp

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

effectFree :: SExp CPSPrim -> Bool
effectFree = cata pureish
  where pureish VarF{}        = True
        pureish LitF{}        = True
        pureish (PrimF p)     = pureOp p
        pureish (IfF i t e)   = i && t && e
        pureish (AppF f args) = and (f : args)
        pureish _             = False
        pureOp (UserPrim Display) = False
        pureOp (UserPrim Exit)    = False
        pureOp Halt               = False
        pureOp _                  = True

shouldInline :: Var -> SExp CPSPrim -> SExp CPSPrim -> Bool
shouldInline var arg body = not (mutated var body) && size arg < 30 && effectFree arg

inlineExp :: SExp CPSPrim -> SExp CPSPrim
inlineExp = cata folder -- Simple inlining, inline only small pure arguments
  where folder (AppF (Lam [v] [e]) [a]) | shouldInline v a e = substitute v a e 
        folder e = embed e
