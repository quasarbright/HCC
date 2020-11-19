module Main where

import Parse
import Compile
import System.Environment


swallow :: Show a => Either a b -> b
swallow (Left err) = error (show err)
swallow (Right x) = x

main :: IO ()
main = do
    (fname:_) <- getArgs
    source <- readFile fname
    let prog = swallow $ parseProgram fname source
    let asm = swallow $ compileStr prog
    putStrLn asm