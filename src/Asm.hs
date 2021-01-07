{-# LANGUAGE LambdaCase #-}

module Asm where

import Data.List (intercalate)


-- | word size in bytes
wordSize :: Integral a => a
wordSize = 8 -- 64 bits

{-
Conventions:
The answer goes in RAX
RCX is used when you need two registers at once, like binary operations
-}
data Reg = RAX | RBP | RSP | RCX | RDX deriving(Eq, Ord, Show)

data Arg = Const Integer
         | Reg Reg
         | RegOffset Reg Integer -- offset is in words
         deriving(Eq, Ord)

data Instr = IMov Arg Arg
           | ILea Arg Arg -- load effective address. like x = &y
           | IAdd Arg Arg
           | ISub Arg Arg
           | IMul Arg Arg
           | IAnd Arg Arg
           | IOr Arg Arg
           | INot Arg
           | INeg Arg
           | IPush Arg
           | IPop Arg
           | IRet
           | IAnnot Instr String
           | IComment String
           deriving(Eq, Ord)


instance Show Arg where
    show = \case
        Const n -> show n
        Reg r -> show r
        RegOffset r n
            | n > 0 -> "["++show r++" + "++show (n * wordSize)++"]"
            | n < 0 -> "["++show r++" - "++show (-n * wordSize)++"]"
            | otherwise -> "["++show r++"]"

showBinInstr :: (Show a1, Show a2) => [Char] -> a1 -> a2 -> [Char]
showBinInstr name l r = "    "++name++" "++show l++", "++show r

showUnInstr :: Show a => [Char] -> a -> [Char]
showUnInstr name arg = "    "++name++" "++show arg

instance Show Instr where
    show = \case
        IMov dest@RegOffset{} source@Const{} -> showBinInstr "mov qword" dest source
        IMov dest source -> showBinInstr "mov" dest source
        ILea dest source -> showBinInstr "lea" dest source
        IAdd dest rhs -> showBinInstr "add" dest rhs
        ISub dest rhs -> showBinInstr "sub" dest rhs
        IMul dest rhs -> showBinInstr "imul" dest rhs
        IAnd dest rhs -> showBinInstr "and" dest rhs
        IOr dest rhs -> showBinInstr "or" dest rhs
        INot arg -> showUnInstr "not" arg
        INeg arg -> showUnInstr "neg" arg
        IPush arg -> showUnInstr "push" arg
        IPop arg -> showUnInstr "pop" arg
        IRet -> "    ret"
        IAnnot i c -> show i ++ " ; "++c
        IComment c -> "    ; "++c
    showList = showString . intercalate "\n" . fmap show