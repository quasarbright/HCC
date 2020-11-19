{-# LANGUAGE LambdaCase #-}

module Asm where

import Data.List (intercalate)


-- | word size in bytes
wordSize :: Integral a => a
wordSize = 8 -- 64 bits

data Reg = RAX | RBP | RSP deriving(Eq, Ord, Show)

data Arg = Const Integer
         | Reg Reg
         | RegOffset Reg Integer
         deriving(Eq, Ord)

data Instr = IMov Arg Arg
           | IAdd Arg Arg
           | ISub Arg Arg
           | IRet
           deriving(Eq, Ord)


instance Show Arg where
    show = \case
        Const n -> show n
        Reg r -> show r
        RegOffset r n
            | n > 0 -> "["++show r++" + "++show (n * wordSize)++"]"
            | n < 0 -> "["++show r++" - "++show (-n * wordSize)++"]"
            | otherwise -> "["++show r++"]"


instance Show Instr where
    show = \case
        IMov dest source -> "    mov "++show dest++", "++show source
        IAdd dest rhs -> "    add "++show dest++", "++show rhs
        ISub dest rhs -> "    sub "++show dest++", "++show rhs
        IRet -> "    ret"
    showList = showString . intercalate "\n" . fmap show