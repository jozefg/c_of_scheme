module CPS where
import Control.Applicative
import Gen
import AST
import Error

cpsifySDec :: [SDec UserPrim] -> FailGen [SDec CPSPrim]
cpsifySDec decs = mapM toCPS decs
  where toCPS (Def v l@(Lam{}))        = Def v <$> cps l (Prim Halt)
        toCPS (Def v (Lit (SInt 0))) = return $ Def v (Lit (SInt 0))
        toCPS (Def v e) = 
          Def v <$> (freshLam (\res -> return $ Set v res) >>= cps e)

(#) :: SExp p -> SExp p -> SExp p
f # v = App f [v]

runAll :: [SExp UserPrim] -> ([SExp CPSPrim] -> FailGen (SExp CPSPrim)) -> FailGen (SExp CPSPrim)
runAll = go []
  where go cpsed [] f = f (reverse cpsed)
        go cpsed (e:es) f = (freshLam $ \e' -> go (e':cpsed) es f) >>= cps e

cps :: SExp UserPrim -> SExp CPSPrim -> FailGen (SExp CPSPrim)
cps (Lit l) k = return $ k # Lit l
cps (Set v e) k = (freshLam $ \r -> return $ k # Set v r) >>= cps e 
cps (Lam args exps) k = do
  newCont <- Gen <$> gen
  (k#) <$> newLam newCont
    where newLam newCont =
            Lam (newCont : args) . (:[]) <$>
            runAll exps (return . (Var newCont#) . head) -- run all expressions and return the result
cps (Var v) k = return $ k # Var v
cps (If test true false) k =
  cont >>= cps test
  where cont = freshLam $ \ test' ->
          If test' <$> cps true k <*> cps false k
cps (App (Lam vars exps) args) k = unfoldArgs vars args
  where unfoldArgs (v : vars) (a : args) = unfoldArgs vars args >>= cps a . Lam [v] . (:[])
        unfoldArgs [] [] = runAll exps (return . (k#) . head)
        unfoldArgs _ _ =
          failCPS "cps" "Mismatch arguments in CPS conversion for literal lambda application"
cps (App f args) k = runAll args $ useArgs f
  where useArgs (Prim p) cArgs = return $ k # App (Prim $ UserPrim p) cArgs
        useArgs f        cArgs = freshLam (\f' -> return $ App f' (k:cArgs)) >>= cps f
cps (Prim CallCC) k = do
  cont' <- cont
  freshLam $ \f -> return $ f `App` [k, cont']
  where cont = do
          k'      <- Gen <$> gen
          result  <- Gen <$> gen
          return $ Lam [k', result] [k # Var result]
cps (Prim p) k = return $ k # Prim (UserPrim p)
