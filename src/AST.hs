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

data Expr a = EInt Integer a
            | EVar String a
            | EUnop Unop (Expr a) a
            | EBinop Binop (Expr a) (Expr a) a
            deriving(Eq, Ord)

instance Show (Expr a) where
    show = \case
        EInt n _ -> show n
        EVar x _ -> x
        EUnop op e _ -> show op++"("++show e++")"
        EBinop op left right _ -> "("++show left++" "++show op++" "++show right++")"

getTag :: Expr p -> p
getTag = \case
    EInt _ a -> a
    EVar _ a -> a
    EUnop _ _ a -> a
    EBinop _ _ _ a -> a

data Statement a = Assign String (Expr a) a
                 | Return (Expr a) a
                 deriving(Eq, Ord)

instance Show (Statement a) where
    show = \case
        Assign x rhs _ -> x++" = "++show rhs++";"
        Return e _ -> "return "++show e++";"
    showList = showString . intercalate "\n" . fmap show

data Program a = Program [Statement a] a deriving (Eq, Ord)

instance Show (Program a) where
    show (Program stmts _) = show stmts
    

-- | how many stack slots need to be allocated for the given list of statements?
numVars :: [Statement a] -> Int
numVars stmts = length $ nub [x | Assign x _ _ <- stmts]
