module Main where
import AST
import CPS
import ClosureConvert
main :: IO ()
main = putStrLn "Cool stuff we'll implement"

inner = Lam [SVar "x"] [] [Prim (UserPrim Plus) `App` [Var $ SVar "x", Var $ SVar "i"]]
outer = Lam [SVar "i"] [Def (SVar "f") inner] [Var (SVar "f") `App` [Lit $ SInt 1]]
test = [Def (SVar "foo") outer]
