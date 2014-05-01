module RewriteToplevels where

import Control.Monad.Writer
import Error
import Gen
import AST
import Control.Applicative

type Rewrite = WriterT [(Var, SExp UserPrim)] FailGen


makeMain :: [SDec UserPrim] -> FailGen [SDec UserPrim]
makeMain = ap (make . Gen <$> gen) . runWriterT . mapM rewriteDec
  where make name (decs, exps) = decs ++ [Def name $ buildLam exps]

buildLam :: [(Var, SExp UserPrim)] -> SExp UserPrim
buildLam = flip App [] . Lam [] . map (uncurry Set)

rewriteDec :: SDec UserPrim -> Rewrite (SDec UserPrim)
rewriteDec d@(Def _ Lam{}) = return d -- Don't rewrite functions
rewriteDec (Def v e)       = tell [(v, e)] >> return (Def v (Lit $ SInt 0))
