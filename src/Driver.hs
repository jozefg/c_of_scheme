module Driver
       ( parseFile
       , parseString
       , compileScheme
       , compileC
       , renderC
       , compiler
       ,) where
import Control.Monad
import Control.Error
import System.Posix.User
import System.Cmd
import Language.C.DSL (pretty, CExtDecl)
import Utils.Error
import AST
import Parser
import RewriteToplevels
import CPS
import OptimizeCPS
import ClosureConvert
import CodeGen

-- | The big compilation function, chains together each section of the
-- compiler and returns a list of C declarations
compileScheme :: [SDec UserPrim] -> Compiler [CExtDecl]
compileScheme = addPrimops >=> makeMain >=> cpsify >=> optimizeCPS >=> closConvert >=> codegen
  where addPrimops = return . (++prims)

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
  print output -- Prints ExitSuccess or failure, the only output of the compiler ATM (usability!)

-- | The IO part of the compiler, handling loading and parsing files
-- and joining together all the compilation glue of compileScheme and friends
compiler :: [FilePath] -> IO ()
compiler files = do
  res <- mapM parseFile files :: IO ([Compiler [SDec UserPrim]])
  let compRes = runCompiler (joinFiles res >>= compileScheme)
  case  compRes of
    Right source -> compileC source
    Left  e      -> errLn (presentError e)
  where joinFiles = fmap concat . sequence

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
