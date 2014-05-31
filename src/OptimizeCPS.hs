module OptimizeCPS (optimize) where
import AST
import Data.Functor.Foldable

optimize :: Compiler (SExp CPSPrim) -> Compiler (SExp CPSPrim)
optimize = id -- Go go fast
