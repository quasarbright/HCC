module Main where

import Parse
import Compile
import Typing
import qualified WellFormedness as WF
import System.Environment

swallowS :: Either String a -> a
swallowS (Left err) = error err
swallowS (Right x) = x

swallow :: Show a => Either a b -> b
swallow (Left err) = errorWithoutStackTrace (show err)
swallow (Right x) = x

go fname = do
    source <-  readFile fname
    let prog = swallowS $ parseProgram fname source
    let progWF = swallow (prog <$ WF.checkWellformedness prog)
    let progTY = swallow (prog <$ typeCheckProgram progWF)
    let asm = swallow $ compileStr progTY
    putStrLn asm

main :: IO ()
main = do
    (fname:_) <- getArgs
    go fname