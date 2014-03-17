{-# LANGUAGE OverloadedStrings #-}
module CodeGen where
import Gen
import AST
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Language.C.DSL
import Data.String

type CodeGenM = StateT (M.Map Var String) Gen

codegen :: [SDec ClosPrim] -> [CExtDecl]
codegen = runGen . flip evalStateT (M.empty) . mapM generateSDec

mangle :: Var -> CodeGenM String
mangle v = do
  res <- M.lookup v <$> get
  case res of
    Nothing | isLegal v -> modify (M.insert v (show v)) >> return (show v)
            | otherwise -> makeLegal >>= modify . M.insert v >> (M.! v) <$> get
    Just s -> return s
  where isLegal (SVar{}) = True
        isLegal _        = False
        makeLegal = ("__" ++) . show <$> gen 

generate :: SExp ClosPrim -> CodeGenM CExpr
generate (Var v) = fromString <$> mangle v
generate (App f args) = (#) <$> generate f <*> mapM generate args
generate (If test true false) = ternary <$> generate test <*> generate true <*> generate false
generate (Prim (NewClos v)) = do
  name <- mangle v
  return . fromString $ "<<primitive: NewClos " ++ name ++ ">>"
generate (Prim p) = return . fromString $ "<<primitive: " ++ show p ++ ">>"
generate (Lit (SInt i))  = return $ "mkInt"#[fromInteger . toInteger $ i]
generate (Lit (SSym s))  = return $ "mkSym"#[fromString $ s]
generate (Set v e) = ("set"#) . (:[]) <$> generate e
generate Lam{} = error "Hey you've found a lambda in a bad spot. CRY TEARS OF BLOOD"

generateSDec :: SDec ClosPrim -> CodeGenM CExtDecl
generateSDec (Def v (Lam args exps)) = do
  funName <- fromString <$> mangle v
  vars <- map (int . fromString) <$> mapM mangle args
  body <- mapM generate exps
  return . export $ fun [voidTy] funName vars (hBlock body)
generateSDec (Def v (App _ [e])) = do
  name <- fromString <$> mangle v
  body <- generate e
  return . export $ int name .= body
generateSDec (Def v e) = do
  name <- fromString <$> mangle v
  body <- generate e
  return . export $ int name .= body
