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

type ClosPath = M.Map Var [Int]
type ClosVar  = M.Map Var (SExp ClosPrim)
type ClosM = StateT ClosVar (ReaderT ClosPath Gen)
type SExpM = ClosM (SExp ClosPrim)

runClosM :: ClosM [SDec ClosPrim]  -> Gen [SDec ClosPrim]
runClosM = fmap combine
           . flip runReaderT M.empty
           . flip runStateT M.empty
  where combine (results, closVars) = map (uncurry Def) (M.toList closVars) ++ results

convert :: Gen [SDec CPSPrim] -> Gen [SDec ClosPrim]
convert decs = runClosM $ do
  undefinedClos <- Gen <$> gen
  newDecs <- lift (lift decs) >>= convertDecs undefinedClos
  return $ (Def undefinedClos (Set undefinedClos $ Prim TopClos)) : newDecs

isCloseVar :: Var -> ClosM Bool
isCloseVar v = M.member v <$> ask

isCloseFun :: Var -> ClosM Bool
isCloseFun v = M.member v <$> get

closeOver :: Var -> SExp CPSPrim -> SExpM
closeOver _ (Prim p) = return $ Prim (CPSPrim p)
closeOver c (Var v)  = do
  closedVar <- isCloseVar v
  closedFun <- isCloseFun v
  if closedVar then do
             path <- (M.! v) <$> ask
             return . Prim $ SelectClos path c
    else if closedFun then
           return $ Prim MkLam `App` [Var c, Var v]
         else return $ Var v
closeOver _ (Lit l) = return $ Lit l
closeOver c (App f args) = handleFun <$> closeOver c f <*>  mapM (closeOver c) args
  where handleFun (Var v) = App (Prim MkLam `App` [Prim TopClos, Var v])
        handleFun p       = App p
closeOver c (If test true false) = If <$> closeOver c test
                                      <*> closeOver c true
                                      <*> closeOver c false
closeOver c (Set v exp) = do
  closedVar <- closeOver c (Var v)
  case closedVar of
    Var v'  -> Set v' <$> closeOver c exp
    Prim x  -> App (Prim WriteClos) <$> sequence [return $ Prim x, closeOver c exp]
    Lam _ [App (Var v') _] -> Set v' <$> closeOver c exp
closeOver c (Lam vars exps) = do
  lambdaName <- Gen <$> gen
  lifted <- liftedLam
  modify (M.insert lambdaName lifted)
  return $ Var lambdaName
  where liftedLam = do
          closName <- Gen <$> gen
          newClos  <- Gen <$> gen
          exps'    <- addClos $ mapM (closeOver newClos) exps
          return . Lam (closName : vars) $
            Prim (NewClos newClos) `App` (map Var $ closName : vars) : exps'
        addClos m = local (M.union newVars . M.map (0:)) $ m
        newVars = M.fromList $ zip vars (map pure [1..])

convertDecs :: Var -> [SDec CPSPrim] -> ClosM [SDec ClosPrim]
convertDecs c = mapM convertDec
  where convertDec (Def n (App (Prim Halt) [(Lam vars exps)])) = do
          newClos  <- Gen <$> gen
          exps'    <- addClos vars $ mapM (closeOver newClos) exps
          return . Def n .  Lam vars $ 
            Prim (NewClos newClos) `App` (map Var $ c : vars) : exps'
        convertDec (Def n e) = Def n <$> closeOver c e
        addClos vars m = local (M.union (newVars vars) . M.map (0:)) $ m
        newVars vars = M.fromList $ zip vars (map pure [1..])

