module RewriteToplevels where

import Control.Monad.Writer
import Error
import Gen
import AST
import Control.Applicative

type Rewrite = WriterT [(Var, SExp UserPrim)] FailGen


rewrite :: [SDec UserPrim] -> FailGen [SDec UserPrim]
rewrite = ap (make . Gen <$> gen) . runWriterT . mapM rewriteDec
  where make name (decs, exps) = decs ++ [Def name $ makeMain exps]

makeMain :: [(Var, SExp UserPrim)] -> SExp UserPrim
makeMain = Lam [] . map (uncurry Set)

rewriteDec :: SDec UserPrim -> Rewrite (SDec UserPrim)
rewriteDec d@(Def _ Lam{}) = return d -- Don't rewrite functions
rewriteDec (Def v e)       = tell [(v, e)] >> return (Def v (Lit $ SInt 0))
