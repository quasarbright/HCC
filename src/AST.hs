{-# LANGUAGE LambdaCase #-}

module AST where

import Data.List (intercalate, nub)


data Unop = Deref | AddrOf | Neg | Not | Inv deriving(Eq, Ord)

instance Show Unop where
    show = \case
        Deref -> "*"
        AddrOf -> "&"
        Neg -> "-"
        Not -> "!"
        Inv -> "~"

data Binop = Plus | Times | BitAnd | BitOr | Eq deriving(Eq, Ord)

instance Show Binop where
    show = \case
        Plus -> "+"
        Times -> "*"
        BitAnd -> "&"
        BitOr -> "|"
        Eq -> "=="

data Expr = EInt Integer
            | EVar String
            | EUnop Unop Expr 
            | EBinop Binop Expr Expr
            deriving(Eq, Ord)

instance Show Expr where
    show = \case
        EInt n -> show n
        EVar x -> x
        EUnop op e -> show op++"("++show e++")"
        EBinop op left right -> "("++show left++" "++show op++" "++show right++")"

data Statement = Assign String Expr
                 | Return Expr
                 deriving(Eq, Ord)

instance Show Statement where
    show = \case
        Assign x rhs -> x++" = "++show rhs++";"
        Return e -> "return "++show e++";"
    showList = showString . intercalate "\n" . fmap show

newtype Program = Program [Statement] deriving (Eq, Ord)

instance Show Program where
    show (Program stmts) = show stmts
    

-- | how many stack slots need to be allocated for the given list of statements?
numVars :: [Statement] -> Int
numVars stmts = length $ nub [x | Assign x _ <- stmts]
