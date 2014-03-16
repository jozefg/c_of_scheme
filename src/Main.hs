module Main where
import System.Environment
import System.Exit
import Data.Either
import AST
import CPS
import Gen
import ClosureConvert
import Parser

main :: IO ()
main = do
  [file] <- getArgs
  parseRes <- parseFile file
  ast <- either ((>>exitWith (ExitFailure 1)) . print) return parseRes
  print ast
  putStr "\n\n\n******CPS******\n"
  let cps = cpsifySDec ast
  print $ runGen cps
  putStr "\n\n\n******Closure******\n"
  let cast = convert cps
  print cast
  
