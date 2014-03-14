module ClosureConvert where
import AST
import Gen
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

-- Procedure for closure conversion:
--        
-- Lift each lambda to it's own toplevel function accepting an
-- extra parameter, a closure. Add it to a list of closured functions
--        
-- Maintain a list of local variables and a list of closure converted variables
-- and look up each explicitly as needed.
--
-- At the beginning of each lambda, define a new value to be the new closure
-- and pass it to all functions marked as needing it.

freeVars :: SExp a -> [Var]
freeVars = go []
  where go bound (Var v) = if v `elem` bound then [] else [v]
        go bound (Lam vars decs es) = es >>= go (vars ++ bound) -- Warning, doesn't deal with declarations yet
        go bound (App f args) = f : args >>= go bound
        go bound (Set v e) = [Var v, e] >>= go bound
        go bound (If test true false) = [test, true, false] >>= go bound
        go bound (Prim{}) = []
        go bound (Lit{}) = []

type ClosPath = M.Map Var [Int]
type ClosVar  = M.Map Var (SExp ClosPrim)
type ClosM = StateT ClosVar (ReaderT ClosPath Gen)
type SExpM = ClosM (SExp ClosPrim)

isCloseVar :: Var -> ClosM Bool
isCloseVar v = M.member v <$> ask
isCloseFun v = M.member v <$> get

newClos :: SExp ClosPrim -> [Var] -> (SExp ClosPrim -> SExpM) -> SExpM
newClos previous vars f = do
  local (M.union newVars . M.map (0:)) $
    f (Prim NewClos `App` (previous : map Var vars))
  where newVars = M.fromList $ zip vars (map pure [1..])

closeOver :: Var -> SExp CPSPrim -> SExpM
closeOver c (Prim p) = return $ Prim (CPSPrim p)
closeOver c (Var v)  = do
  closedVar <- isCloseVar v
  closedFun <- isCloseFun v
  if closedVar then do
             path <- (M.! v) <$> ask
             let lookupVar = foldr (\i c -> Prim SelectClos `App` [Lit $ SInt i, c]) (Var c) path
             return lookupVar
  else if closedFun then do
                  (Lam (_:vars) _ _) <- (M.! v) <$> get
                  return $ Lam vars [] [Var v `App` map Var (c : tail vars)]
    else return $ Var v
closeOver _ (Lit l) = return $ Lit l
closeOver c (App f args) = App <$> closeOver c f <*> mapM (closeOver c) args
closeOver c (If test true false) = If <$> closeOver c test
                                      <*> closeOver c true
                                      <*> closeOver c false
closeOver c (Set v exp) = do
  closedVar <- closeOver c (Var v)
  case closedVar of
    Var v'  -> Set v' <$> closeOver c exp
    Prim x  -> App (Prim WriteClos) <$> sequence [return $ Prim x, closeOver c exp]
    Lam _ _ [App (Var v') _] -> Set v' <$> closeOver c exp
closeOver c (Lam vars decs exps) = do
  lambdaName <- Gen <$> gen
  lifted <- liftedLam
  modify (M.insert lambdaName lifted)
  return $ Lam vars [] [Var lambdaName `App` map Var vars]
  where liftedLam = do
          closName <- Gen <$> gen
          newClos (Var closName) vars $ \c' -> do
            decs' <- convertDecs closName decs
            exps' <- mapM (closeOver closName) exps
            return $ Lam (closName : vars) (Def closName c' : decs') exps'

convertDecs :: Var -> [SDec CPSPrim] -> ClosM [SDec ClosPrim]
convertDecs c = mapM convertDec
  where convertDec (Def n e) = Def n <$> closeOver c e
    
