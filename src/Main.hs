module Main where
import AST
import CPS
import ClosureConvert
import CodeGen
import Parser
import Language.C.DSL (pretty)
import System.Environment

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

main :: IO ()
main = do
  [file] <- getArgs
  res <- parseFile file
  case res of
    Left err -> print err
    Right ast -> print ast >> writeFile "out.c" (compile ast)
