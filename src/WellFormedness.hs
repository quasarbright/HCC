{-# LANGUAGE LambdaCase #-}

module WellFormedness where

import AST
import Type

import Control.Monad.RWS.Strict
import Data.Function ((&))
import Data.Functor (($>))


type Env = [String] -- variables in scope

data WFError = UnboundVar String
             | DupVar String
             | UnexpectedArrayLiteral Expr
             | BadAddrOf Expr
             | NegativeIndex Expr
             | VoidVar String
             | VoidReturn String
             | MissingReturn String
             | UnreachableStatement Statement
             | InternalError String
             deriving(Eq, Show)

type Checker a = (RWS Env [WFError] () a)

-- utility

nothing :: Monad m => m ()
nothing = return ()

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
    EInt{} -> nothing
    EVar x -> do
        b <- varBound x
        if b then nothing else throwError (UnboundVar x)
    EUnop AddrOf e -> (if isLHS e
                       then nothing
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
    EApp f args -> mapM_ checkExpr (f:args)

checkLHS :: LHS -> Checker ()
checkLHS = checkExpr . exprOfLhs

checkDups :: [String] -> Checker ()
checkDups vars = mapM_ throwError errs
    where
        errs = reverse $ go vars
        go [] = []
        go (x:xs)
            | x `elem` xs = DupVar x:go xs
            | otherwise = go xs

-- | only checks one level (doesn't go into ifs)
getDeclaredVars :: [Statement] -> [String]
getDeclaredVars b = reverse $ b >>= \case
    Assign{} -> []
    Return{} -> []
    Decl _ x -> [x]
    Def _ x _ -> [x]
    If{} -> [] -- don't go deep
    While{} -> []
    For{} -> []

checkBlock :: [Statement] -> Checker ()
checkBlock b = checkDups (getDeclaredVars b) >> go b
    where
        go [] = nothing
        go (stmt:rest) = let mRest = go rest in case stmt of
            Assign lhs rhs -> checkLHS lhs >> checkExpr rhs >> mRest
            Return e ->  checkExpr e >> checkUnreachable >> mRest
                where checkUnreachable = case rest of
                        [] -> nothing
                        next:_ -> throwError (UnreachableStatement next)
            Decl _ x -> addVar x mRest
            -- need this special case because array literals are not well formed
            Def _ x (EArrayLiteral es) -> mapM_ checkExpr es >> addVar x mRest
            Def _ x rhs -> checkExpr rhs >> addVar x mRest
            If cnd thn mEls -> do
                checkExpr cnd
                checkBlock thn
                maybe nothing checkBlock mEls
                mRest
            While cnd body -> checkExpr cnd >> checkBlock body >> mRest
            For setup cnd update body ->
                checkBlock (whileOfFor setup cnd update body) >> mRest -- var not in scope for rest!

checkSignature :: Type -> String -> [(Type, String)] -> Checker ()
checkSignature _ _ targs = checkDups (snd <$> targs) >> checkVoids
    where checkVoids = mapM_ (uncurry go) targs
          go TVoid x = throwError (VoidVar x)
          go _ _ = nothing

-- | does the block of statements definitely return?
doesReturn :: [Statement] -> Bool
doesReturn = any go
    where go = \case
            Return{} -> True
            If _ _ Nothing -> False
            If _ thn (Just els) -> doesReturn thn && doesReturn els
            -- setup may be return
            For setup cnd update body -> doesReturn (whileOfFor setup cnd update body)
            While{} -> False
            Assign{} -> False
            Decl{} -> False
            Def{} -> False

-- | is it possible for the block of statements to return?
mayReturn :: [Statement] -> Bool
mayReturn = any go
    where go = \case
            Return{} -> True
            If _ thn mEls -> mayReturn thn || maybe False mayReturn mEls
            For setup cnd update body -> mayReturn (whileOfFor setup cnd update body)
            While _ body -> mayReturn body
            Assign{} -> False
            Decl{} -> False
            Def{} -> False

checkTopDecls :: [TopDecl] -> Checker ()
checkTopDecls decls = checkDups (getDeclVars decls) >> checkDups (getDefVars decls) >> foldr go nothing decls
    where
        -- this separation is necessary bc you can declare, then define
        getDeclVars [] = []
        getDeclVars (FunDef{}:ds) = getDeclVars ds
        getDeclVars (FunDecl _ f _:ds) = f:getDeclVars ds
        getDefVars [] = []
        getDefVars (FunDecl{}:ds) = getDefVars ds
        getDefVars (FunDef _ f _ _:ds) = f:getDefVars ds
        go decl mRest = case decl of
            FunDecl _ f targs -> mapM_ go' targs >> addVar f mRest
                where go' = \case
                        (TVoid, x) -> throwError (VoidVar x)
                        (TInt{},_) -> nothing
                        (TRef{},_) -> nothing
                        (TArray{},_) -> nothing
                        (TFun{},_) -> nothing
            FunDef ret f targs body -> do
                go (FunDecl ret f targs) nothing
                checkDups (fmap snd targs)
                case ret of
                    TVoid | mayReturn body -> throwError (VoidReturn f)
                          | otherwise -> nothing
                    _ | doesReturn body -> nothing
                      | otherwise -> throwError (MissingReturn f)
                -- f in scope for recursion!
                -- f last to reflect the precedence of args over the function for recursion
                addVars (fmap snd targs++[f]) $ checkBlock body
                addVar f mRest

checkProgram :: Program -> Checker ()
checkProgram (Program decls) = checkTopDecls decls

-- running the monad

executeChecker :: Env -> Checker () -> [WFError]
executeChecker env m = snd $ execRWS m env ()

wrapResult :: [a] -> Either [a] ()
wrapResult [] = Right ()
wrapResult errs = Left errs

checkWellformedness :: Program -> Either [WFError] Program
checkWellformedness prog = (prog & checkProgram & executeChecker [] & wrapResult) $> prog
