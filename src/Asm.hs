{-# LANGUAGE LambdaCase #-}

module Asm where

import Data.List (intercalate)


-- | word size in bytes
wordSize :: Integer
wordSize = 8 -- 64 bits

data Reg = RAX deriving(Eq, Ord)

data Arg = Const Integer
         | Reg Reg
         | RegOffset Reg Integer
         deriving(Eq, Ord)

data Instr = IMov Arg Arg
           | IRet
           deriving(Eq, Ord)
           
instance Show Reg where
    show = \case
        RAX -> "RAX"

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
        IRet -> "    ret"
    showList = showString . intercalate "\n" . fmap show