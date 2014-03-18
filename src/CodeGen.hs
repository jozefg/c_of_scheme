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

scm_t :: CDeclr -> Maybe CExpr -> CDecl
scm_t = decl (CTypeSpec (CTypeDef "scm_t" undefNode))

codegen :: [SDec ClosPrim] -> [CExtDecl]
codegen = runGen . flip evalStateT (M.empty) . fmap concat . mapM generateSDec

mangle :: Var -> CodeGenM String
mangle v = do
  res <- M.lookup v <$> get
  case res of
    Nothing -> makeLegal >>= modify . M.insert v >> (M.! v) <$> get
    Just s  -> return s
  where makeLegal = ("__" ++) . show <$> gen

generate :: SExp ClosPrim -> CodeGenM CExpr
generate (Var v) = fromString <$> mangle v
generate (If test true false) = ternary <$> fmap isZero (generate test) <*> generate false <*> generate true
  where isZero e = "scm_eq_raw"#[e, "mkInt"#[0]]
generate (App (Prim (NewClos v)) args) = do
  name <- fromString . ("scm_t "++) <$> mangle v
  escaping <- mapM generate args
  return $ name <-- "mkClos"# (numArgs:escaping)
  where numArgs = fromInteger . toInteger . length $ args
generate (Prim SelectClos) = return "scm_select_clos"
generate (Prim WriteClos)  = return "scm_write_clos"
generate (Prim (CPSPrim Halt)) = return "scm_halt"
generate (Prim (CPSPrim (UserPrim p))) = return $ case p of
  AST.Plus -> "scm_plus"
  Sub  -> "scm_sub"
  Mult -> "scm_mult"
  Div  -> "scm_div"
  Eq   -> "scm_eq"
  Cons -> "scm_cons"
  Car  -> "scm_car"
  Cdr  -> "scm_cdr"
  Display -> "display"
generate (Prim TopClos) = return $ "scm_top_clos"
generate (Lit (SInt i))  = return $ "mkInt"#[fromInteger . toInteger $ i]
generate (Lit (SSym s))  = return $ "mkSym"#[fromString $ s]
generate (App f@(Var{}) args) = ("scm_apply"#) . (numArgs:) <$> mapM generate (f:args)
  where numArgs = fromInteger . toInteger . length $ args
generate (App f@(Prim{}) args) = (#) <$> generate f <*> mapM generate args
generate (Set v e) = (<--) <$> fmap fromString (mangle v) <*> generate e
generate Lam{} = error "Hey you've found a lambda in a bad spot. CRY TEARS OF BLOOD"

generateSDec :: SDec ClosPrim -> CodeGenM [CExtDecl]
generateSDec (Def v (Lam args exps)) = do
  varName <- fromString <$> mangle v
  funName   <- Gen <$> gen >>= mangle
  arrayName <- Gen <$> gen >>= mangle
  vars <- map (scm_t . fromString) <$> mapM mangle args
  body <- map intoB <$> mapM generate exps
  let init = zipWith (assignFrom $ fromString arrayName) vars [0..]
      var  = scm_t varName .= "mkLam"#[fromString funName]
      lam  = fun [voidTy] (fromString funName) [scm_t . ptr $ fromString arrayName] $
             block $ init ++ head body : intoB ("free"#[fromString arrayName]) : tail body
  return $ [export lam, export var]
  where assignFrom arr var i = intoB $ var .= (arr ! fromInteger i)
generateSDec (Def v (App _ [e])) = do
  name <- fromString <$> mangle v
  body <- generate e
  return [export $ scm_t name .= body]
generateSDec (Def v e) = do
  name <- fromString <$> mangle v
  body <- generate e
  return [export $ scm_t name .= body]
