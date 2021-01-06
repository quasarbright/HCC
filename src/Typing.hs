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

type Env = Map String Type

data TypeError = Mismatch Type Type
               | UnboundVar String
               | BadDeref Type
               | InternalError String
               | BadVoid Expr
               deriving(Eq, Ord)

instance Show TypeError where
    show = \case
        Mismatch e a -> "Type Mismatch: Expected "++show e++", but got "++show a
        UnboundVar x -> "Unbound variable: "++ x
        BadDeref t -> "Cannot dereference value of type "++show t
        InternalError s -> "Internal type checking error: "++s
        BadVoid e -> "Value cannot be void: "++show e

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
        Nothing -> throwError (UnboundVar x)

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
        EUnop AddrOf (EVar x) -> TRef <$> lookupVar x
        EUnop AddrOf _ -> throwError (InternalError "bad addrOf at typecheck")
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

inferLHS :: LHS -> Checker Type
inferLHS =
    let toExpr (LVar x) = EVar x
        toExpr (LDeref lhs) = EUnop Deref (toExpr lhs)
    in inferExpr . toExpr

checkBlock :: Type -> [Statement] -> Checker ()
checkBlock t b = assertEqual t =<< inferBlock b

inferBlock :: [Statement] -> Checker Type
inferBlock = foldr go (return TVoid)
    where
        go stmt mRest =
            case stmt of
                Assign lhs rhs -> do
                    tL <- inferLHS lhs
                    tR <- inferExpr rhs
                    assertEqual tL tR
                    mRest
                Decl t x -> annot x t mRest
                Def t x rhs -> checkExpr t rhs >> annot x t mRest
                Return e -> inferExpr e <* mRest -- rest should be empty                
            
            
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


    