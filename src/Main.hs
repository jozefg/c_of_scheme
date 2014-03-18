module Main where
import System.Environment
import System.Exit
import Data.Either
import AST
import CPS
import Gen
import ClosureConvert
import CodeGen

main = return ()

{- A quick test, meant to be run cabal repl
   (define x 1)
   (define y 1)

   (define foo
     ((lambda (i) (+ x i))
      1))
-}
     
man = Def (SVar "main") (Prim Display `App` [Lit $ SInt 0])
