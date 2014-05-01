module RewriteToplevels where

import Control.Monad.Writer
import Control.Monad.State
import Error
import Gen
import AST
import Control.Applicative

type Rewrite = WriterT [(Var, SExp UserPrim)] Compiler


makeMain :: [SDec UserPrim] -> Compiler [SDec UserPrim]
makeMain = (>>=make) . runWriterT . mapM rewriteDec
  where make (decs, exps) = do
          lVar <- Gen <$> gen 
          put lVar
          return $ decs ++ [Def lVar $ buildLam exps]

buildLam :: [(Var, SExp UserPrim)] -> SExp UserPrim
buildLam = Lam [] . map (uncurry Set)

rewriteDec :: SDec UserPrim -> Rewrite (SDec UserPrim)
rewriteDec d@(Def _ Lam{}) = return d -- Don't rewrite functions
rewriteDec (Def v e)       = tell [(v, e)] >> return (Def v (Lit $ SInt 0))
