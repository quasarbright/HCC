module Compile where

import AST
import Asm
import Data.List (intercalate)

compile :: Program -> [Instr]
compile (Program n) = [IMov (Reg RAX) (Const n), IRet]

compileStr :: Program -> String
compileStr p =
    let pStr = show (compile p) in
    intercalate "\n" [ "section .text"
            , "global our_code_starts_here"
            , "our_code_starts_here:"
            , pStr
            ]