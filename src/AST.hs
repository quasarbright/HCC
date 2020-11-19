{-# LANGUAGE LambdaCase #-}

module AST where

import Data.List (intercalate, nub)


data Expr a = EInt Integer a
            | EVar String a
            deriving(Eq, Ord)

instance Show (Expr a) where
    show = \case
        EInt n _ -> show n
        EVar x _ -> x


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
