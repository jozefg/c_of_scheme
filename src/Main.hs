module Main where
import System.Environment
import Driver

main :: IO ()
main = getArgs >>= compiler

