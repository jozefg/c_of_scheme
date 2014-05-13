module Main where
import AST
import CPS
import RewriteToplevels
import ClosureConvert
import CodeGen
import Parser
import Gen
import Error
import Language.C.DSL (pretty)

import Text.Parsec (ParseError)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Error

import System.Environment
import System.Posix.User
import System.Cmd

-- | The big compilation function, chains together each section of the
-- compiler and returns either a failure or the C code as a string.
compile :: Either ParseError [SDec UserPrim] -> Either Failure String
compile =  runGen
          . eitherT (return . Left) success
          . flip evalStateT (SVar "")
          . (codegen <=< closConvert <=< cpsify <=< makeMain <=< (lift . hoistEither))
          . fmap (++prims)
          . intoFail
  where success = return
                  . Right
                  . ("#include <stdlib.h>\n#include \"rts.h\"\n"++)
                  . unlines
                  . map (show . pretty)
        intoFail (Left e)  = Left $ Failure Parser "parseSDec" (show e)
        intoFail (Right r) = Right r

-- | A hacky way to automatically compile the C
-- code using GCC, assumes the RTS is in ~/.c_of_scheme/
compileC :: String -> IO ()
compileC code = do
  UserEntry{homeDirectory = hd} <- getRealUserID >>= getUserEntryForID
  let cFile = "out.c"
      cBits = hd ++ "/.c_of_scheme/"
      rts   = cBits ++ "rts.c"
      gc    = cBits ++ "gc.c"
  writeFile cFile code
  output <- system $ "gcc -O3 -I" ++ unwords [cBits, cFile, rts, gc] ++ "  `pkg-config --cflags --libs glib-2.0`"
  print output

main :: IO ()
main = do
  files <- getArgs
  res <- mapM parseFile files
  case compile (fmap concat . sequence $ res) of
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
