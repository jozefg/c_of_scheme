{-# LANGUAGE OverloadedStrings #-}
module CodeGen where
import Gen
import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import qualified Data.Map as M
import Language.C.DSL
import Data.String
import Data.Foldable (foldMap)
import Data.Maybe (catMaybes)

type CodeGenM = WriterT [(CDecl, String, CExpr)] (StateT (M.Map Var String) Gen)

scm_t :: CDeclr -> Maybe CExpr -> CDecl
scm_t = decl (CTypeSpec (CTypeDef "scm_t" undefNode))

codegen :: Gen [SDec ClosPrim] -> [CExtDecl]
codegen = runGen
          . flip evalStateT (M.empty)
          . fmap (uncurry makeMain)
          . runWriterT
          . fmap catMaybes
          . (mapM generateSDec <=< lift . lift)
  where makeMain decls inits = makePrototypes inits ++ decls ++ [export $ fun [intTy] "main"[] (makeBlock inits)]
        makeBlock = hBlock . foldMap (\(_, var, expr) -> [fromString var <-- expr])
        makePrototypes = map export . map (\(a, _, _) -> a)

mangle :: Var -> CodeGenM String
mangle v = do
  res <- M.lookup v <$> get
  case res of
    Nothing -> makeLegal >>= modify . M.insert v >> (M.! v) <$> get
    Just s  -> return s
  where makeLegal = ("__" ++) . show <$> gen

generate :: SExp ClosPrim -> CodeGenM CExpr
generate (Var v) = fromString <$> mangle v
generate (If test true false) = ternary <$> fmap isZero (generate test) <*> generate true <*> generate false
  where isZero e = Not `pre` "scm_eq_raw"#[e, "mkInt"#[0]]
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
generate (Lit (SSym s))  = return $ "mkSym"#[fromString $ show s]
generate (App f@(Var{}) args) = ("scm_apply"#) . (numArgs:) <$> mapM generate (f:args)
  where numArgs = fromInteger . toInteger . length $ args
generate (App f@(Prim{}) args) = (#) <$> generate f <*> mapM generate args
generate (Set v e) = (<--) <$> fmap fromString (mangle v) <*> generate e
generate Lam{} = error "Hey you've found a lambda in a bad spot. CRY TEARS OF BLOOD"

generateSDec :: SDec ClosPrim -> CodeGenM (Maybe CExtDecl)
generateSDec (Def v (Lam args exps)) = do
  varName   <- mangle v
  funName   <- Gen <$> gen >>= mangle
  arrayName <- Gen <$> gen >>= mangle
  vars <- map (scm_t . fromString) <$> mapM mangle args
  body <- map intoB <$> mapM generate exps
  -- Export the variable containing the lambdas
  tell [(scm_t (fromString varName) Nothing, varName, "mkLam"#[fromString funName])]
  -- Build the corresponding function
  let init = zipWith (assignFrom $ fromString arrayName) vars [0..]
      lam  = fun [voidTy] (fromString funName) [scm_t . ptr $ fromString arrayName] $
             block $ init ++ head body : intoB ("free"#[fromString arrayName]) : tail body
  return . Just $ export lam
  where assignFrom arr var i = intoB $ var .= (arr ! fromInteger i)
generateSDec (Def v (App _ [e])) = do
  name <- mangle v
  body <- generate e
  tell [(scm_t (fromString name) Nothing, name, body)]
  return Nothing
generateSDec (Def v e) = do
  name <- mangle v
  body <- generate e
  tell [(scm_t (fromString name) Nothing, name, body)]
  return Nothing

