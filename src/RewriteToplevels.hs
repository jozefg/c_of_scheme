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
          return $ decs ++ [Fun lVar [] $ buildLam exps]

buildLam :: [(Var, SExp UserPrim)] -> [SExp UserPrim]
buildLam = (++ [Prim Exit]) . map (uncurry Set)

rewriteDec :: SDec UserPrim -> Rewrite (SDec UserPrim)
rewriteDec (Def v (Lam vars exps)) = return (Fun v vars exps) 
rewriteDec (Def v e)              = tell [(v, e)] >> return (Init v)
