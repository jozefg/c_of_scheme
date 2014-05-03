module ClosureConvert where
import AST
import Gen
import Error
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
type GlobalVars  = S.Set Var
type Closures = [(Var, SExp ClosPrim)]
type ClosM    = StateT GlobalVars (WriterT Closures (ReaderT ClosPath Compiler))
type SExpM    = ClosM (SExp ClosPrim)

runClosM :: GlobalVars -> ClosM [SDec ClosPrim]  -> Compiler [SDec ClosPrim]
runClosM decNames = fmap combine
                    . flip runReaderT M.empty
                    . runWriterT
                    . flip evalStateT decNames
  where combine (results, closVars) = map build closVars ++ results
        build (v, Lam vars exps) = Fun v vars exps
        

convert :: [SDec CPSPrim] -> Compiler [SDec ClosPrim]
convert decs = runClosM (buildEnv decs) $ do
  undefinedClos <- Gen <$> gen
  newDecs       <- convertDecs undefinedClos decs
  return $ (Def undefinedClos $ Prim TopClos) : newDecs
  where buildEnv = foldr addEnv S.empty . filter notLam
        notLam Fun{} = False
        notLam _     = True
        addEnv (Init n) m    = S.insert n m
        addEnv (Fun n _ _) m = S.insert n m

isCloseVar :: Var -> ClosM Bool
isCloseVar v = M.member v <$> ask

isGlobalVar :: Var -> ClosM Bool
isGlobalVar v = S.member v <$> get

writeToClos :: [Int] -> SExp CPSPrim -> Var -> SExpM
writeToClos path exp c =
  App (Prim WriteClos) <$>
  sequence [return . Lit  $ SInt (last path)
           ,closeOver c exp
           ,return . Prim $ SelectClos (init path) c]

closeVar :: Var {- Actual var -} -> Var {- Continuation var -} -> SExpM
closeVar v c = do
  closedVar <- isCloseVar v
  globalVar <- isGlobalVar v
  if closedVar then do
             path <- (M.! v) <$> ask
             return . Prim $ SelectClos path c
    else if globalVar then
           return $ Var v
         else return $ Prim MkLam `App` [Var c, Var v]
              -- Note, we supply all functions with the current closure
              -- top level functions will simply discard it.return $ Var v


closeOver :: Var -> SExp CPSPrim -> SExpM
closeOver _ (Prim p) = return $ Prim (CPSPrim p)
closeOver c (Var v)  = closeVar v c
closeOver _ (Lit l) = return $ Lit l
closeOver c (App f args) = App <$> closeOver c f <*> mapM (closeOver c) args
closeOver c (If test true false) = If <$> closeOver c test
                                      <*> closeOver c true
                                      <*> closeOver c false
closeOver c (Set v exp) = do
  closedVar <- closeOver c (Var v)
  case closedVar of
    Var v'  -> Set v' <$> closeOver c exp
    Prim (SelectClos path c) -> writeToClos path exp c
    Lam _ [App (Var v') _] -> Set v' <$> closeOver c exp
closeOver c (Lam vars exps) = do
  lambdaName <- Gen <$> gen
  lifted <- liftedLam
  tell [(lambdaName, lifted)]
  return $ Prim MkLam `App` [Var c, Var lambdaName]
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
  where convertDec (Fun n vars exps) = do
          newClos  <- Gen <$> gen
          oldClos  <- Gen <$> gen
          exps'    <- addClos vars $ mapM (closeOver newClos) exps
          return . Fun n (oldClos:vars) $ 
            Prim (NewClos newClos) `App` (map Var $ c : vars) : exps'
        convertDec (Init v)  = return $ Init v
        convertDec (Def _ _) = failClos "convertDec" "Found an unrewritten Dec!" 
        addClos vars m = local (M.union (newVars vars) . M.map (0:)) $ m
        newVars vars = M.fromList $ zip vars (map pure [1..])
