{-# LANGUAGE OverloadedStrings #-}
module CodeGen (codegen) where
import Gen
import Error
import AST
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import qualified Data.Map as M
import Language.C.DSL
import Data.String
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (foldl')

type CodeGenM = WriterT [(CDecl, Maybe String, CExpr)] (StateT (M.Map Var String) Compiler)

scm_t :: CDeclr -> Maybe CExpr -> CDecl
scm_t = decl (CTypeSpec (CTypeDef "scm_t" undefNode))

codegen :: [SDec ClosPrim] -> Compiler [CExtDecl]
codegen = flip evalStateT (M.empty)
          . fmap (uncurry makeMain)
          . runWriterT
          . fmap catMaybes
          . (<*tellMain)
          . mapM generateSDec

makeMain :: [CExtDecl] -> [(CDecl, Maybe String, CExpr)] -> [CExtDecl]
makeMain decls inits =
  mapMaybe makeFunProt decls
  ++ map makeProt inits
  ++ map gccFriendly decls
  ++ [export $ fun [intTy] "main"[] (makeBlock inits)]
  where makeBlock = hBlock . concatMap buildExp
        buildExp (_, v, expr) = maybe [expr] ((:[]) . (<--expr) . fromString) v
        makeProt = export . (\(a, _, _) -> a)

gccFriendly :: CExtDecl -> CExtDecl
gccFriendly (CFDefExt (CFunDef specs decl [] stat a)) =
  case decl of
    CDeclr i d n _ a' -> CFDefExt (CFunDef specs (CDeclr i d n [] a') [] stat a)
gccFriendly a = a

makeFunProt :: CExtDecl -> Maybe CExtDecl
makeFunProt (CFDefExt (CFunDef specs declr _ _ a)) =
  Just . export $ CDecl specs [(Just declr, Nothing, Nothing)] a
makeFunProt _                                      = Nothing

generateSDec :: SDec ClosPrim -> CodeGenM (Maybe CExtDecl)
generateSDec (Fun v args exps) = do
  varName   <- mangle v
  arrayName <- Gen <$> gen >>= mangle
  vars <- map (scm_t . fromString) <$> mapM mangle args
  body <- map intoB <$> mapM generate exps
  -- Build the corresponding function
  let inits = zipWith (assignFrom $ fromString arrayName) vars [0..]
      lam  = annotatedFun
             [voidTy]
             (fromString varName)
             [scm_t . ptr $ fromString arrayName]
             ["noreturn"] $
             block $ inits ++ head body : intoB ("free"#[fromString arrayName]) : tail body
  return . Just $ export lam
  where assignFrom arr var i = intoB $ var .= (arr ! fromInteger i)
generateSDec (Init v) = simpleSDec v 0
generateSDec (Def v (Prim TopClos)) = simpleSDec v "scm_top_clos"
generateSDec d = failGen "generateSDec" $ "Unmatched case for" ++ show d

simpleSDec :: Var -> CExpr -> CodeGenM (Maybe a)
simpleSDec v e = do
  name <- mangle v
  tell [(scm_t (fromString name) Nothing, Just name, e)]
  return Nothing

mangle :: Var -> CodeGenM String
mangle v = do
  res <- M.lookup v <$> get
  case res of
    Nothing -> makeLegal >>= modify . M.insert v >> (M.! v) <$> get
    Just s  -> return s
  where makeLegal = ("__" ++) . show <$> gen

cLen :: [a] -> CExpr
cLen = fromInteger . toInteger . length

generate :: SExp ClosPrim -> CodeGenM CExpr
generate (Var v) = fromString <$> mangle v
generate (If test b1 b2) = ternary <$> (isZ <$> generate test) <*> generate b1 <*> generate b2
  where isZ e = Not `pre` ("scm_eq_raw"#[e, "mkInt"#[0]])
generate (App (App (Prim MkLam) closArgs) args) = generateLam closArgs args
generate (App (Prim MkLam) closArgs) = ("mkLam"#) <$> mapM generate closArgs 
generate (App (Prim (NewClos v)) args) = generateClos v args
generate (App f@(Var{}) args) = ("scm_apply"#) . (cLen args:) <$> mapM generate (f:args)
generate (App f@(Prim (SelectClos{})) args) = ("scm_apply"#) . (cLen args:) <$> mapM generate (f:args)
generate (App f args) = (#) <$> generate f <*> mapM generate args
generate (Set v e) = (<--) <$> fmap fromString (mangle v) <*> generate e
generate (Lit l)   = generateLit l
generate (Prim p)  = generatePrim p
generate Lam{}     = failGen "generate" "unlifted lambda in a bad spot"

-- | Generate a new lambda applied to some arguments
generateLam :: [SExp ClosPrim] -> [SExp ClosPrim] -> CodeGenM CExpr
generateLam closArgs args = do
  closArgs' <- mapM generate closArgs
  args'     <- mapM generate args
  return $ "scm_apply"#([cLen args, "mkLam"#closArgs'] ++ args')

-- | Generate a new closure applied to some arguments
generateClos :: Var -> [SExp ClosPrim] -> CodeGenM CExpr
generateClos v args = do
  name <- fromString . ("scm_t "++) <$> mangle v
  escaping <- mapM generate args
  return $ name <-- "mkClos"# (cLen args : escaping) -- A closure needs to know its length

-- | Generate primitives
generatePrim :: ClosPrim -> CodeGenM CExpr
generatePrim (CPSPrim (UserPrim p)) = generateUserPrim p
generatePrim (CPSPrim Halt)         = return $ "scm_halt"
generatePrim TopClos                = return $ "scm_top_clos"
generatePrim WriteClos              = return "scm_write_clos"
generatePrim (SelectClos path v) =  makePath path <$> generate (Var v)
  where toCInt = fromInteger . toInteger
        makePath = flip . foldl' $ \cexpr i -> "scm_select_clos"#[toCInt i, cexpr]
generatePrim _  = failGen "generatePrim" "found a Prim that should have been generated earlier"

-- | The massive switch to generate the rts calls for primops
generateUserPrim :: UserPrim -> CodeGenM CExpr
generateUserPrim p = case p of
  AST.Plus -> return "scm_plus"
  Sub  -> return  "scm_sub"
  Mult -> return  "scm_mult"
  Div  -> return  "scm_div"
  Eq   -> return  "scm_eq"
  Cons -> return  "scm_cons"
  Car  -> return  "scm_car"
  Cdr  -> return  "scm_cdr"
  Display -> return  "display"
  Exit    -> return $ "scm_stop"#[]

-- | Literals generations
generateLit :: SLit -> CodeGenM CExpr
generateLit (SInt i)  = return $ "mkInt"#[fromInteger . toInteger $ i]
generateLit (SSym s)  = return $ "mkSym"#[fromString $ show s]

tellMain :: CodeGenM ()
tellMain = do
  mainVar <- lift . lift $ get
  main <- mangle mainVar
  tell [(int "_" Nothing, Nothing, "scm_init"#[fromString main])]
