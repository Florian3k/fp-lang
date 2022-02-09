module Main where

import Eval (evalProgram)
import Parser (programParser)
import System.Environment (getArgs)
import System.IO (isEOF)
import Text.Megaparsec (errorBundlePretty, parse, parseTest)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> do
      file <- readFile fname
      case parse programParser fname file of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right prog -> evalProgram prog
    _ -> putStrLn "Usage: fp-lang <filename>"
