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
               | AppliedNonFunction Type
               | ArityError Int Int
               deriving(Eq)

instance Show TypeError where
    show = \case
        Mismatch e a -> "Type Mismatch: Expected "++show e++", but got "++show a
        TUnboundVar x -> "Unbound variable: "++ x
        BadDeref t -> "Cannot dereference value of type "++show t
        InternalError s -> "Internal type checking error: "++s
        BadVoid e -> "Value cannot be void: "++show e
        BadArrayAccess t -> "Cannot do array access on a value of type "++show t
        AppliedNonFunction t -> "Called a non-function: "++show t
        ArityError expected actual -> "Arity error: expected "++show expected++" arguments, but got "++show actual

newtype Checker a = Checker { runChecker :: ExceptT TypeError (Reader Env) a }
                 deriving( Functor
                         , Applicative
                         , Monad
                         , MonadReader Env
                         , MonadError TypeError
                         )

-- utility --

nothing :: Monad m => m ()
nothing = return ()

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
                TFun{} -> throwError (BadDeref t)
        EUnop AddrOf e -> maybe err (const (TRef <$> inferExpr e)) (lhsOfExpr e)
            where err = throwError (InternalError "bad addrOf at typecheck")
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
                TFun{} -> err
        EArrayLiteral [] -> throwError (InternalError "cannot infer type of empty array literal")
        EArrayLiteral (e:es) -> do
            t <- inferExpr e
            checkExpr t (EArrayLiteral es)
            return (TArray t Nothing)
        EApp f args -> do
            tF <- inferExpr f
            case tF of
                TFun ret argts -> do
                    let nExpected = length argts
                    let nActual = length args
                    unless (nExpected == nActual) (throwError (ArityError nExpected nActual))
                    zipWithM_ checkExpr argts args
                    return ret
                _ -> throwError (AppliedNonFunction tF)   

inferLHS :: LHS -> Checker Type
inferLHS = inferExpr . exprOfLhs

-- | ensure a statement is either an effect or returns the given type
checkStatement :: Type -> Statement -> Checker ()
checkStatement tExpected stmt = case stmt of
     Assign lhs rhs -> do
         t <- inferLHS lhs
         checkExpr t rhs
     Decl{} -> nothing
     Def t _ rhs -> checkExpr t rhs
     Return e -> checkExpr tExpected e
     If cnd thn mEls -> do
         checkExpr TInt cnd
         checkBlock tExpected thn
         maybe nothing (checkBlock tExpected) mEls
     While cnd body -> do
         checkExpr TInt cnd
         checkBlock tExpected body
     For setup cnd update body -> checkBlock tExpected (whileOfFor setup cnd update body)
    

checkBlock :: Type ->  [Statement] -> Checker ()
checkBlock tExpected = go
    where
        go [] = nothing
        go (stmt:rest) =
            let mRest = go rest
                mStmt = checkStatement tExpected stmt
            in mStmt >> case stmt of
                Assign{} -> mRest
                Return{} -> mRest
                Decl t x -> annot x t mRest
                Def t x _ -> annot x t mRest
                If{} -> mRest
                While{} -> mRest
                For{} -> mRest
                
checkDecl :: TopDecl -> Checker ()
checkDecl = \case
    FunDecl{} -> nothing
    FunDef ret f targs body -> do
        let tF = TFun ret (fmap fst targs)
        -- f last bc args have scope precedence: void f(int x, int f) {body}, f :: int in body
        let annotations = [(x,t) | (t,x) <- targs++[(tF,f)]]
        annots annotations (checkBlock ret body)
        

checkDecls :: [TopDecl] -> Checker ()
checkDecls [] = nothing
checkDecls (decl:rest) =
    let mRest = checkDecls rest
        mDecl = checkDecl decl
    in mDecl >> case decl of
        FunDecl ret f targs -> annot f (TFun ret (fmap fst targs)) mRest
        FunDef ret f targs _ -> annot f (TFun ret (fmap fst targs)) mRest
            
checkProgram :: Program -> Checker ()
checkProgram (Program decls) = checkDecls decls
        

-- monad running --

executeChecker :: Env -> Checker a -> Either TypeError a
executeChecker env =
    runChecker
    >>> runExceptT
    >>> flip runReader env

typeCheckProgram :: Program -> Either TypeError ()
typeCheckProgram = executeChecker Map.empty . void . checkProgram


    