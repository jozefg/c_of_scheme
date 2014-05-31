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
