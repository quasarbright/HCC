{-# LANGUAGE LambdaCase #-}

module WellFormedness where

import AST

import Control.Monad.RWS.Strict
import Data.Function ((&))
import Data.Functor (($>))


type Env = [String] -- variables in scope

data WFError = UnboundVar String
             | DupVar String
             | UnexpectedArrayLiteral Expr
             | BadAddrOf Expr
             | NegativeIndex Expr
             | InternalError String
             deriving(Eq, Ord, Show)

type Checker a = (RWS Env [WFError] () a)

-- utility

varBound :: String -> Checker Bool
varBound x = asks (x `elem`)

addVar :: String -> Checker a -> Checker a
addVar x = local (x:)

addVars :: Foldable t => t String -> Checker a -> Checker a
addVars = flip (foldr addVar)

throwError :: WFError -> Checker ()
throwError = tell . (:[])

-- checking

checkExpr :: Expr -> Checker ()
checkExpr = \case
    EInt{} -> return ()
    EVar x -> do
        b <- varBound x
        if b then return () else throwError (UnboundVar x)
    EUnop AddrOf e -> (if isLHS e
                       then return ()
                       else throwError (BadAddrOf e))
                      >> checkExpr e
    EUnop Deref e -> checkExpr e
    EUnop Neg e -> checkExpr e
    EUnop Not e -> checkExpr e
    EUnop Inv e -> checkExpr e
    EBinop _ l r -> checkExpr l >> checkExpr r
    e@(EGetIndex arr (EUnop Neg (EInt n))) | n > 0 -> checkExpr arr >> throwError (NegativeIndex e)
    EGetIndex arr idx -> checkExpr arr >> checkExpr idx
    e@(EArrayLiteral es) -> throwError (UnexpectedArrayLiteral e) >> mapM_ checkExpr es

checkLHS :: LHS -> Checker ()
checkLHS = checkExpr . exprOfLhs

checkDup :: String -> Checker ()
checkDup x = do
    b <- varBound x
    when b $ throwError (DupVar x)

-- TODO check all paths return
-- TODO unreachable statements (need paramorphism)
checkBlock :: [Statement] -> Checker ()
checkBlock = foldr go (return ())
    where
        go stmt mRest = case stmt of
            Assign lhs rhs -> checkLHS lhs >> checkExpr rhs >> mRest
            Return e -> checkExpr e >> mRest
            Decl _ x -> checkDup x >> addVar x mRest
            Def _ x (EArrayLiteral es) -> checkDup x >> mapM_ checkExpr es
            Def _ x rhs -> checkDup x >> checkExpr rhs >> addVar x mRest

checkProgram :: Program -> Checker ()
checkProgram (Program block) = checkBlock block

-- running the monad

executeChecker :: Env -> Checker () -> [WFError]
executeChecker env m = snd $ execRWS m env ()

wrapResult :: [a] -> Either [a] ()
wrapResult [] = Right ()
wrapResult errs = Left errs

checkWellformedness :: Program -> Either [WFError] Program
checkWellformedness prog = (prog & checkProgram & executeChecker [] & wrapResult) $> prog
