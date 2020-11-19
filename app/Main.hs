module Main where

import Parse
import Compile
import System.Environment


main :: IO ()
main = do
    (fname:_) <- getArgs
    source <- readFile fname
    let prog = parseProgram source
    let asm = compileStr prog
    putStrLn asm
    
