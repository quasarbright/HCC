{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Typing where

import AST
import Type

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow ((>>>))
import Data.Functor (($>))

type Env = Map String Type

data TypeError = Mismatch Type Type
               | TUnboundVar String
               | BadDeref Type
               | InternalError String
               | BadVoid Expr
               | BadArrayAccess Type
               deriving(Eq)

instance Show TypeError where
    show = \case
        Mismatch e a -> "Type Mismatch: Expected "++show e++", but got "++show a
        TUnboundVar x -> "Unbound variable: "++ x
        BadDeref t -> "Cannot dereference value of type "++show t
        InternalError s -> "Internal type checking error: "++s
        BadVoid e -> "Value cannot be void: "++show e
        BadArrayAccess t -> "Cannot do array access on a value of type "++show t

newtype Checker a = Checker { runChecker :: ExceptT TypeError (Reader Env) a }
                 deriving( Functor
                         , Applicative
                         , Monad
                         , MonadReader Env
                         , MonadError TypeError
                         )

-- utility --

lookupVar :: String -> Checker Type
lookupVar x = do
    v <- asks (Map.lookup x)
    case v of
        Just v' -> return v'
        Nothing -> throwError (TUnboundVar x)

annot :: String -> Type -> Checker a -> Checker a
annot x t = local (Map.insert x t)

annots :: [(String, Type)] -> Checker a -> Checker a
annots = flip $ foldr (uncurry annot)

assertEqual :: Type -> Type -> Checker ()
assertEqual expected actual
    | expected == actual = return ()
    | otherwise = throwError (Mismatch expected actual)

-- checker --

checkExpr :: Type -> Expr -> Checker ()
checkExpr TArray{} (EArrayLiteral []) = return ()
checkExpr (TArray t _) (EArrayLiteral es) = mapM_ (checkExpr t) es
checkExpr t e = assertEqual t =<< inferExpr e

inferExpr :: Expr -> Checker Type
inferExpr = 
    let unop e = checkExpr TInt e >> return TInt
        binop left right = mapM_ (checkExpr TInt) [left, right] >> return TInt
    in \case
        EInt{} -> return TInt
        EVar x -> lookupVar x
        EUnop Deref e -> do
            t <- inferExpr e
            case t of
                TRef t' -> return t'
                TInt -> throwError (BadDeref t)
                TVoid -> throwError (BadDeref t)
                TArray{} -> throwError (BadDeref t) -- TODO treat as t*?
        EUnop AddrOf e -> maybe err (const (TRef <$> inferExpr e)) (lhsOfExpr e)
            where err = throwError (InternalError "bad addrOf at typecheck")
            -- TODO test more relaxed addrOf
        EUnop Neg e -> unop e
        EUnop Not e -> unop e
        EUnop Inv e -> unop e
        EBinop Plus l r -> binop l r
        EBinop Times l r -> binop l r
        EBinop BitAnd l r -> binop l r
        EBinop BitOr l r -> binop l r
        EBinop Eq l r -> do
            tL <- inferExpr l
            tR <- inferExpr r
            if TVoid `elem` [tL, tR]
            then throwError (BadVoid (EBinop Eq l r))
            else assertEqual tL tR >> return TInt
        EGetIndex arr idx -> do
            tArr <- inferExpr arr
            let err = throwError (BadArrayAccess tArr)
            case tArr of
                TArray t _ -> checkExpr TInt idx $> t
                TRef{} -> err
                TInt{} -> err
                TVoid{} -> err
        EArrayLiteral [] -> throwError (InternalError "cannot infer type of empty array literal")
        EArrayLiteral (e:es) -> do
            t <- inferExpr e
            checkExpr t (EArrayLiteral es)
            return (TArray t Nothing)          

inferLHS :: LHS -> Checker Type
inferLHS = inferExpr . exprOfLhs

checkBlock :: Type -> [Statement] -> Checker ()
checkBlock t b = assertEqual t =<< inferBlock b

inferBlock :: [Statement] -> Checker Type
inferBlock = foldr go (return TVoid)
    where
        go stmt mRest =
            case stmt of
                -- TODO for if else if() t else void is ok!!!
                Assign lhs rhs -> do
                    t <- inferLHS lhs
                    checkExpr t rhs
                    mRest
                Decl t x -> annot x t mRest
                Def t x rhs -> checkExpr t rhs >> annot x t mRest
                Return e -> inferExpr e <* mRest -- rest should be empty                
                If cnd thn Nothing -> do
                    checkExpr TInt cnd
                    tThn <- inferBlock thn
                    case tThn of
                        TVoid -> mRest
                        -- TODO use paramorphism and checkBlock
                        _ -> (assertEqual tThn =<< mRest) >> return tThn
                If cnd thn (Just els) -> do
                    checkExpr TInt cnd
                    tThn <- inferBlock thn
                    tEls <- inferBlock els
                    case (tThn, tEls) of
                        (TVoid,TVoid) -> mRest
                        (t, TVoid) -> (assertEqual t =<< mRest) >> return t
                        (TVoid, t) -> (assertEqual t =<< mRest) >> return t
                        _ -> do
                            assertEqual tThn tEls
                            -- TODO use paramorphism and checkBlock
                            assertEqual tThn =<< mRest
                            return tThn
                While cnd body -> do
                    checkExpr TInt cnd
                    tBody <- inferBlock body
                    assertEqual tBody =<< mRest -- TODO change after paramorphism
                    return tBody
                -- TODO change after paramorphism
                For setup cnd update body -> inferBlock (whileOfFor setup cnd update body) >> mRest
                    

            
checkProgram :: Type -> Program -> Checker ()
checkProgram t (Program b) = checkBlock t b
        
inferProgram :: Program -> Checker Type
inferProgram (Program b) = inferBlock b

executeChecker :: Env -> Checker a -> Either TypeError a
executeChecker env =
    runChecker
    >>> runExceptT
    >>> flip runReader env

typeCheckProgram :: Program -> Either TypeError ()
typeCheckProgram = executeChecker Map.empty . void . inferProgram


    