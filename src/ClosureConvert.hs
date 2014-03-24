module ClosureConvert where
import AST
import Gen
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
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
type FunVars  = S.Set Var
type Closures = [(Var, SExp ClosPrim)]
type ClosM    = StateT FunVars (WriterT Closures (ReaderT ClosPath Gen))
type SExpM    = ClosM (SExp ClosPrim)

runClosM :: Gen FunVars -> ClosM [SDec ClosPrim]  -> Gen [SDec ClosPrim]
runClosM decNames decs = decNames >>= flip runClos decs
  where combine (results, closVars) = map (uncurry Def) closVars ++ results
        runClos :: FunVars -> ClosM [SDec ClosPrim]  -> Gen [SDec ClosPrim]
        runClos decNames' = fmap combine
                            . flip runReaderT M.empty
                            . runWriterT
                            . flip evalStateT decNames'
        

convert :: Gen [SDec CPSPrim] -> Gen [SDec ClosPrim]
convert decs = runClosM (buildEnv <$> decs) $ do
  undefinedClos <- Gen <$> gen
  newDecs <- (lift . lift . lift) decs >>= convertDecs undefinedClos
  return $ (Def undefinedClos (Set undefinedClos $ Prim TopClos)) : newDecs
  where buildEnv = foldr addEnv S.empty . filter isLam
        isLam (Def _ (App (Prim Halt) [Lam{}])) = True
        isLam _                                           = False
        addEnv (Def n (App (Prim Halt) [Lam{}])) m =
          S.insert n m

isCloseVar :: Var -> ClosM Bool
isCloseVar v = M.member v <$> ask

isCloseFun :: Var -> ClosM Bool
isCloseFun v = S.member v <$> get

closeOver :: Var -> SExp CPSPrim -> SExpM
closeOver _ (Prim p) = return $ Prim (CPSPrim p)
closeOver c (Var v)  = do
  closedVar <- isCloseVar v
  isFun <- isCloseFun v
  if closedVar then do
             path <- (M.! v) <$> ask
             return . Prim $ SelectClos path c
    else if isFun then
           return $ Prim MkLam `App` [Var c, Var v]
           -- Note, we supply all functions with the current closure
           -- top level functions will simply discard it.
         else return $ Var v
closeOver _ (Lit l) = return $ Lit l
closeOver c (App f args) = handleFun <$> closeOver c f <*>  mapM (closeOver c) args
  where handleFun (Var v) = App (Prim MkLam `App` [Var c, Var v])
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
  tell [(lambdaName, lifted)]
  modify (S.insert lambdaName)
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
          oldClos  <- Gen <$> gen
          exps'    <- addClos vars $ mapM (closeOver newClos) exps
          return . Def n .  Lam (oldClos:vars) $ 
            Prim (NewClos newClos) `App` (map Var $ c : vars) : exps'
        convertDec (Def n e) = Def n <$> closeOver c e
        addClos vars m = local (M.union (newVars vars) . M.map (0:)) $ m
        newVars vars = M.fromList $ zip vars (map pure [1..])

