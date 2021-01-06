module Main where

import Parse
import Compile
import Typing
import System.Environment

swallowS :: Either String a -> a
swallowS (Left err) = error err
swallowS (Right x) = x

swallow :: Show a => Either a b -> b
swallow (Left err) = errorWithoutStackTrace (show err)
swallow (Right x) = x

main :: IO ()
main = do
    (fname:_) <- getArgs
    source <- readFile fname
    let prog = swallowS $ parseProgram fname source
    -- TODO well formedness
    let prog' = swallow (prog <$ typeCheckProgram prog)
    let asm = swallow $ compileStr prog'
    putStrLn asm