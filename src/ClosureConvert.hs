module ClosureConvert where
import AST
import Gen
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.List (foldl')
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

runClosM :: ClosM [(Var, SDec ClosPrim)]  -> Gen [(Var, SDec ClosPrim)]
runClosM = combine <=< flip runReaderT M.empty . flip runStateT M.empty
  where combine (results, closVars) = (++ results) <$> build (M.toList closVars) 
        build = mapM (\(n, e) -> (,) <$> fmap Gen gen <*> pure (Def n e))

convert :: Gen [(Var, SDec CPSPrim)] -> Gen [(Var, SDec ClosPrim)]
convert decs = runClosM $ do
  undefinedClos <- Gen <$> gen
  closMutVar    <- Gen <$> gen
  newDecs <- lift (lift decs) >>= convertDecs undefinedClos
  
  return $ (closMutVar, Def undefinedClos (Set closMutVar $ Prim TopClos)) : newDecs

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
  else if closedFun then do
                  (Lam (_:vars) _) <- (M.! v) <$> get
                  return $ Lam vars [Var v `App` map Var (c : tail vars)]
    else return $ Var v
closeOver _ (Lit l) = return $ Lit l
closeOver c (App f args) = join $ liftM2 handleFun (closeOver c f) (mapM (closeOver c) args)
  where handleFun (Var f) args = do
          res <- M.lookup f <$> get
          case res of
            Just{} ->  return $ Var f `App` (Var c : args)
            Nothing -> return $ Var f `App` args
        handleFun (Prim p) args = return $ Prim p `App` args
        handleFun (Lam{}) _ = error "This is very wrong, ClosureConversion found an unlifted lambda. Time to cry"
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
  where liftedLam :: SExpM; liftedLam = do
          closName <- Gen <$> gen
          newClos  <- Gen <$> gen
          exps'    <- addClos $ mapM (closeOver newClos) exps
          return . Lam (closName : vars) $
            Prim (NewClos newClos) `App` (map Var $ closName : vars) : exps'
        addClos m = local (M.union newVars . M.map (0:)) $ m
        newVars = M.fromList $ zip vars (map pure [1..])

convertDecs :: Var -> [(Var, SDec CPSPrim)] -> ClosM [(Var, SDec ClosPrim)]
convertDecs c = mapM convertDec
  where convertDec (v, Def n e) = (,) v . Def n <$> closeOver c e
