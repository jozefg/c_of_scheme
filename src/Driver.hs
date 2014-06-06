module Driver where
import Control.Monad
import Control.Monad.State
import Control.Error
import System.Posix.User
import System.Cmd
import Language.C.DSL (pretty, CExtDecl)
import Utils.Gen
import Utils.Error
import AST
import Parser
import RewriteToplevels
import CPS
import OptimizeCPS
import ClosureConvert
import CodeGen

compilerPipeline :: [SDec UserPrim] -> Compiler [CExtDecl]
compilerPipeline = makeMain >=> cpsify >=> optimize >=> closConvert >=> codegen

-- | The big compilation function, chains together each section of the
-- compiler and returns either a failure or the C code as a string.
schemeToC :: Compiler [SDec UserPrim] -> Either Failure [CExtDecl]
schemeToC =  runGen
             . eitherT (return . Left) success
             . flip evalStateT (SVar "")
             . (>>=compilerPipeline)
             . fmap (++prims)
  where success = return . Right

-- | Pretty print the C code with proper includes
renderC :: [CExtDecl] -> String
renderC = ("#include <stdlib.h>\n#include \"rts.h\"\n"++) . unlines . map (show . pretty)

-- | A hacky way to automatically compile the C
-- code using GCC, assumes the RTS is in ~/.c_of_scheme/
compileC :: [CExtDecl] -> IO ()
compileC code = do
  UserEntry{homeDirectory = hd} <- getRealUserID >>= getUserEntryForID
  let cFile = "out.c"
      cBits = hd ++ "/.c_of_scheme/"
      rts   = cBits ++ "rts.c"
      gc    = cBits ++ "gc.c"
  writeFile cFile (renderC code)
  output <- system $ "gcc -g -I" ++ unwords [cBits, cFile, rts, gc] ++ "  `pkg-config --cflags --libs glib-2.0`"
  print output

compiler :: [FilePath] -> IO ()
compiler files = do
  res <- mapM parseFile files
  case schemeToC (fmap concat . sequence $ res) of
    Right source -> compileC source
    Left  e      -> errLn (presentError e)

-- | List of primitives wrapped in fully eta-converted
-- functions. These will be properly CPS converted.
prims :: [SDec UserPrim]
prims = [ Def (SVar "+")       $ Lam [a, b] [Plus # [a', b']]
        , Def (SVar "-")       $ Lam [a, b] [Sub #  [a', b']]
        , Def (SVar "*")       $ Lam [a, b] [Mult # [a', b']]
        , Def (SVar "/")       $ Lam [a, b] [Div #  [a', b']]
        , Def (SVar "eq?")     $ Lam [a, b] [Eq # [a', b']]
        , Def (SVar "cons")    $ Lam [a, b] [Cons # [a', b']]
        , Def (SVar "display") $ Lam [a]    [Display # [a']]
        , Def (SVar "car")     $ Lam [a]    [Car # [a']]
        , Def (SVar "cdr")     $ Lam [a]    [Cdr # [a']]]
  where a = SVar "a"
        b = SVar "b"
        a' = Var a
        b' = Var b
        f # args = App (Prim f) args
