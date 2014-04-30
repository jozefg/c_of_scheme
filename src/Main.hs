module Main where
import AST
import CPS
import ClosureConvert
import CodeGen
import Parser
import Language.C.DSL (pretty)

import System.Environment
import System.Posix.User
import System.Cmd

compile :: [SDec UserPrim] -> String
compile = ("#include <stdlib.h>\n#include \"rts.h\"\n"++)
          . unlines
          . map (show . pretty)
          . codegen
          . convert
          . cpsifySDec
          . (prims++)

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
  case res of
    Left err -> print err
    Right ast -> compileC file (compile ast)
