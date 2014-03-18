module Main where
import AST
import CPS
import ClosureConvert
import CodeGen
import Language.C.DSL

compile :: [SDec UserPrim] -> String
compile = unlines
          . map (show . pretty)
          . codegen
          . convert
          . cpsifySDec
demoProgram :: [SDec UserPrim]
demoProgram = [Def (SVar "_") $ Prim Display `App` [Lit $ SSym "Hello World"]]

main :: IO ()
main = putStr $ "#include <stdlib.h>\n#include \"rts.h\"\n" ++ compile demoProgram
