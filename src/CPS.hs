module CPS where
import Control.Applicative
import Gen
import AST

cpsifySDec :: [SDec UserPrim] -> Gen [SDec CPSPrim]
cpsifySDec = mapM (\(Def nm e) -> Def nm <$> cps e (Prim Halt))

(#) :: SExp p -> SExp p -> SExp p
f # v = App f [v]

runAll :: [SExp UserPrim] -> ([SExp CPSPrim] -> Gen (SExp CPSPrim)) -> Gen (SExp CPSPrim)
runAll = go []
  where go cpsed [] f = f cpsed
        go cpsed (e:es) f =
          (freshLam $ \e' -> go (e':cpsed) es f) >>= cps e

cps :: SExp UserPrim -> SExp CPSPrim -> Gen (SExp CPSPrim)
cps (Lit l) k = return $ k # Lit l
cps (Lam args exps) k = do
  newCont <- Gen <$> gen
  (k#) <$> newLam newCont
    where newLam newCont =
            Lam (newCont : args) . (:[]) <$>
            runAll exps (return . (Var newCont#) . head)
            -- run all expressions and return the result
cps (Var v) k = return $ k # Var v
cps (If test true false) k =
  cont >>= cps test
  where cont = freshLam $ \ test' ->
          If test' <$> cps true k <*> cps false k
cps (App f args) k = runAll args $ useArgs f
  where useArgs (Prim p) cArgs = return $ k # App (Prim $ UserPrim p) cArgs
        useArgs f          cArgs =
          freshLam (\f' -> return $ App f' (k:cArgs)) >>= cps f
cps (Prim CallCC) k = do
  cont' <- cont
  freshLam $ \f -> return $ f `App` [k, cont']
  where cont = do
          k'      <- Gen <$> gen
          result  <- Gen <$> gen
          return $ Lam [k', result] [k # Var result]
cps (Prim p) k = return $ k # Prim (UserPrim p)
