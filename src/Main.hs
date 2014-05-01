module Main where
import AST
import CPS
import RewriteToplevels
import ClosureConvert
import CodeGen hiding(makeMain)
import Parser
import Gen
import Error
import Language.C.DSL (pretty)

import Text.Parsec (ParseError)

import Control.Monad
import Control.Error

import System.Environment
import System.Posix.User
import System.Cmd

compile :: Either ParseError [SDec UserPrim] -> Either Failure String
compile =  runGen
          . eitherT (return . Left) success 
          . (codegen <=< convert <=< cpsifySDec <=< makeMain <=< hoistEither)
          . fmap (++prims)
          . intoFail
  where success = return
                  . Right
                  . ("#include <stdlib.h>\n#include \"rts.h\"\n"++)
                  . unlines
                  . map (show . pretty)
        intoFail (Left e)  = Left $ Failure Parser "parseSDec" (show e)
        intoFail (Right r) = Right r

compileC :: String -> String -> IO ()
compileC file code = do
  UserEntry{homeDirectory = hd} <- getRealUserID >>= getUserEntryForID
  let cFile = file ++ ".c"
      cBits = hd ++ "/.scheme2c/"
      rts   = cBits ++ "rts.c"
  writeFile cFile code
  output <- system $ "gcc -I" ++ unwords [cBits, cFile, rts] 
  print output

main :: IO ()
main = do
  [file] <- getArgs
  res <- parseFile file
  case compile res of
    Right source -> compileC file source
    Left  e      -> errLn (presentError e)

prims :: [SDec UserPrim]
prims = [ Def (SVar "+") $ Lam [a, b] [Plus # [a', b']]
        , Def (SVar "-") $ Lam [a, b] [Sub #  [a', b']]
        , Def (SVar "*") $ Lam [a, b] [Mult # [a', b']]
        , Def (SVar "/") $ Lam [a, b] [Div #  [a', b']]
        , Def (SVar "eq?") $ Lam [a, b] [Eq # [a', b']]
        , Def (SVar "cons") $ Lam [a, b] [Cons # [a', b']]
        , Def (SVar "display") $ Lam [a] [Display # [a']]
        , Def (SVar "car") $ Lam [a] [Car # [a']]
        , Def (SVar "cdr") $ Lam [a] [Cdr # [a']]]
  where a = SVar "a"
        b = SVar "b"
        a' = Var a
        b' = Var b
        f # args = App (Prim f) args
