module CPS (cpsify) where
import Control.Applicative
import AST
import Utils.Gen
import Utils.Error

-- | The driver for CPS conversion, leaves @Init@'s alone
-- and fails when given @Def@s. Otherwise it converts
-- a lambda to CPS style.
cpsify :: [SDec UserPrim] -> Compiler [SDec CPSPrim]
cpsify decs = (:) <$> callcc <*> mapM toCPS decs
  where toCPS (Init v) = return $ Init v
        toCPS (Def _ _) = failCPS "cpsifySDec" "Found an unrewritten Def"
        toCPS (Fun v vars exps) = do
          newLam <- cps (Lam vars exps) (Prim Halt)
          case newLam of
            App (Prim Halt) [(Lam vars exps)] -> return $ Fun v vars exps
            _                                 -> failCPS "toCPS" "Unexpected structure for newLam"


callcc :: Compiler (SDec CPSPrim)
callcc = do
  [f, k, k', result] <- map Gen <$> sequence [gen, gen, gen, gen]
  return $ Fun (SVar "call/cc") [k, f]
    [App (Var f) [Var k, Lam [k', result] [Var k # Var result]]]

-- | A simple short hand for application
(#) :: SExp p -> SExp p -> SExp p
f # v = App f [v]

-- | Run all the expressions in a given list and feed it to a first-class continuation.
runAll :: [SExp UserPrim] -> ([SExp CPSPrim] -> Compiler (SExp CPSPrim)) -> Compiler (SExp CPSPrim)
runAll = go []
  where go cpsed [] f = f (reverse cpsed)
        go cpsed (e:es) f = (freshLam $ \e' -> go (e':cpsed) es f) >>= cps e

-- | The big set of CPS conversion rules, we take an expression and a
-- continuation and convert it to CPS.
cps :: SExp UserPrim -> SExp CPSPrim -> Compiler (SExp CPSPrim)
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
cps (Prim p) k = return $ k # Prim (UserPrim p)
