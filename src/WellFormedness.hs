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

checkDups :: [Statement] -> Checker ()
checkDups b = mapM_ throwError errs
    where
        -- | only checks this level (doesn't go into ifs)
        getNewVars = \case
            Assign{} -> []
            Return{} -> []
            Decl _ x -> [x]
            Def _ x _ -> [x]
            If{} -> [] -- don't go deep
            While{} -> []
            For{} -> []
        newVars = reverse $ b >>= getNewVars
        errs = reverse $ go newVars
        go [] = []
        go (x:xs)
            | x `elem` xs = DupVar x:go xs
            | otherwise = go xs



-- TODO check all paths return
-- TODO unreachable statements (need paramorphism)
checkBlock :: [Statement] -> Checker ()
checkBlock b = checkDups b >> foldr go (return ()) b
    where
        go stmt mRest = case stmt of
            Assign lhs rhs -> checkLHS lhs >> checkExpr rhs >> mRest
            Return e -> checkExpr e >> mRest
            Decl _ x -> addVar x mRest
            -- need this special case because array literals are not well formed
            Def _ x (EArrayLiteral es) -> mapM_ checkExpr es >> addVar x mRest
            Def _ x rhs -> checkExpr rhs >> addVar x mRest
            If cnd thn mEls -> do
                checkExpr cnd
                checkBlock thn
                maybe (return ()) checkBlock mEls
                mRest
            While cnd body -> checkExpr cnd >> checkBlock body >> mRest
            For setup cnd update body ->
                checkBlock (whileOfFor setup cnd update body) >> mRest -- var not in scope for rest!

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
