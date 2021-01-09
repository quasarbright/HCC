{-# LANGUAGE LambdaCase #-}

module AST where

import Data.List (intercalate)
import Type
import Data.Maybe (isJust)

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
            | EGetIndex Expr Expr
            | EArrayLiteral [Expr] -- only valid on rhs of array def
            deriving(Eq, Ord)

-- TODO showsPrec
instance Show Expr where
    show = \case
        EInt n -> show n
        EVar x -> x
        EUnop op e -> "("++show op++"("++show e++"))"
        EBinop op left right -> "("++show left++" "++show op++" "++show right++")"
        EGetIndex arr idx -> show arr ++ "["++show idx++"]"
        EArrayLiteral es -> "{"++intercalate ", " (show <$> es)++"}"

-- TODO just use exprs and make assignments expressions
data LHS = LVar String
         | LDeref LHS
         | LSetIndex Expr Expr
         deriving(Eq, Ord)

instance Show LHS where
    show = show . exprOfLhs

lhsOfExpr :: Expr -> Maybe LHS
lhsOfExpr = \case
    EInt{} -> Nothing
    EVar x -> Just $ LVar x
    EUnop Deref e -> LDeref <$> lhsOfExpr e
    EUnop AddrOf _ -> Nothing
    EUnop Neg _ -> Nothing
    EUnop Not _ -> Nothing
    EUnop Inv _ -> Nothing
    EBinop{} -> Nothing
    EGetIndex arr idx -> Just $ LSetIndex arr idx
    EArrayLiteral{} -> Nothing

isLHS :: Expr -> Bool
isLHS = isJust . lhsOfExpr

exprOfLhs :: LHS -> Expr
exprOfLhs = \case
    LVar x -> EVar x
    LDeref lhs -> EUnop Deref (exprOfLhs lhs)
    LSetIndex arr idx -> EGetIndex arr idx

data Statement = Assign LHS Expr
               | Return Expr
               | Decl Type String
               | Def Type String Expr
               | If Expr [Statement] (Maybe [Statement])
               deriving(Eq)

indent :: String -> String
indent s = intercalate "\n" (("  "++) <$> lines s)

instance Show Statement where
    show = \case
        Assign x rhs -> show x++" = "++show rhs++";"
        Return e -> "return "++show e++";"
        Decl t x -> show t++" "++x++";"
        Def t x rhs -> show t++" "++x++" = "++show rhs++";"
        If cnd thn mEls ->
            let elsStr = case mEls of
                    Nothing -> ""
                    Just els -> " else {\n"++indent (show els)++"\n}"
            in "if ("++show cnd++") {\n"++indent (show thn)++"\n}"++elsStr
    showList = showString . intercalate "\n" . fmap show

newtype Program = Program [Statement] deriving (Eq)

instance Show Program where
    show (Program stmts) = show stmts
    

-- | how many stack slots need to be allocated for the given list of statements?
-- (no local scope)
numSlots :: [Statement] -> Int
numSlots stmts = sum (go <$> stmts)
    where
        go = \case
            Assign{} -> 0
            Return{} -> 0
            Decl (TArray _ (Just n)) _ -> fromIntegral n + 1
            Def (TArray _ (Just n)) _ EArrayLiteral{} -> fromIntegral n + 1
            Decl{} -> 1
            Def{} -> 1
            If _ thn mEls -> numSlots thn + maybe 0 numSlots mEls



