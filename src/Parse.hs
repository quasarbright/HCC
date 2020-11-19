module Parse where

import AST

parseProgram :: String -> Program
parseProgram = Program . read