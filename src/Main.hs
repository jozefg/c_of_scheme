module Main where
import System.Environment
import System.Exit
import Data.Either
import AST
import CPS
import Gen
import ClosureConvert


main = return ()

{- A quick test, meant to be run cabal repl
   (define x 1)
   (define y 1)

   (define foo
     ((lambda (i) (+ (+ x y) i))
      1))
-}
     
x = Def (SVar "x") (Lit $ SInt 1)
y = Def (SVar "y") (Lit $ SInt 1)

inner = Lam [SVar "i"] [Prim Plus `App`  [Var (SVar "y"), Var (SVar "i")]]
outer = Def (SVar "foo") $ inner `App` [Lit (SInt 1)]
