module RewriteToplevels (makeMain) where

import Control.Monad.Writer
import Control.Monad.State
import Gen
import Error
import AST
import Control.Applicative

type Rewrite = WriterT [(Var, SExp UserPrim)] Compiler

-- | Build up the main function by moving toplevel
-- constants into @Init@s and putting the initialization
-- in a new main function. We also store the name of this
-- function in the Compiler monad.
makeMain :: [SDec UserPrim] -> Compiler [SDec UserPrim]
makeMain = (>>=make) . runWriterT . mapM rewriteDec
  where make (decs, exps) = do
          lVar <- Gen <$> gen 
          put lVar
          return $ decs ++ [Fun lVar [] $ buildLam exps]

-- | Build a lambda with an explicit exit at the end.
buildLam :: [(Var, SExp UserPrim)] -> [SExp UserPrim]
buildLam = (++ [Prim Exit]) . map (uncurry Set)

-- | Rewrite a simple decl to either a 'Fun' or a 'Init'.
-- This also hints at what needs to be initialized for 'makeMain'.
rewriteDec :: SDec UserPrim -> Rewrite (SDec UserPrim)
rewriteDec (Def v (Lam vars exps)) = return (Fun v vars exps) 
rewriteDec (Def v e)               = tell [(v, e)] >> return (Init v)
rewriteDec _                       = failRW "rewriteDec" "found unexpected value, not a Dec!"
