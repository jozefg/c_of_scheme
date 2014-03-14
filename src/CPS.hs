module CPS where
import Control.Applicative
import Gen
import AST

cpsifySDec :: [SDec UserPrim] -> [SDec CPSPrim]
cpsifySDec = runGen
             . mapM (\(Def nm e) -> Def nm <$> cps e (Prim Halt))

(#) :: SExp p -> SExp p -> SExp p
f # v = App f [v]

runAll :: [SExp UserPrim] -> ([SExp CPSPrim] -> Gen (SExp CPSPrim)) -> Gen (SExp CPSPrim)
runAll = go []
  where go cpsed [] f = f cpsed
        go cpsed (e:es) f =
          (freshLam $ \e' -> go (e':cpsed) es f) >>= cps e

cps :: SExp UserPrim -> SExp CPSPrim -> Gen (SExp CPSPrim)
cps (Lit l) k = return $ k # Lit l
cps (Lam args decs exps) k = do
  newCont <- Gen <$> gen
  (k#) <$> newLam newCont
    where newLam newCont =
            Lam (newCont : args) (map toCPS decs) . (:[]) <$>
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
cps (Prim p) k = return $ k # Prim (UserPrim p)

toCPS :: SDec UserPrim -> SDec CPSPrim
toCPS (Def n e) = Def n $ go e
  where go (Prim p) = Prim $ UserPrim p
        go (Lit l)  = Lit l
        go (Var v)  = Var v
        go (If test true false) = If (go test) (go true) (go false)
        go (App f args) = go f `App` map go args
        go (Set v e) = Set v $ go e
        go (Lam vs ds es) = Lam vs (map toCPS ds) (map go es)
