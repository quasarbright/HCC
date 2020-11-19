{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Compile where

import AST
import Asm

import Data.List (intercalate)
import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State.Strict

-- | maps names to stack indices, next available stack index
type Env = (Map String Integer, Integer)

emptyEnv :: Env
emptyEnv = (Map.empty, 1)

newtype Compiler a = Compiler { runCompiler :: ExceptT String (State Env) a }
    deriving( Functor
            , Applicative
            , Monad
            , MonadState Env
            , MonadError String
            )

lookupVar :: String -> Compiler Integer
lookupVar x = do
    (m,_) <- get
    case Map.lookup x m of
        Nothing -> throwError "unbound variable"
        Just v -> return v

-- | allocate a stack slot for the new variable if we haven't seen it. Otherwise, return its slot
allocVar :: String -> Compiler Integer
allocVar x = do
    (m,nextIndex) <- get
    case Map.lookup x m of
        Nothing -> do
            put (Map.insert x nextIndex m, succ nextIndex)
            return nextIndex
        Just index -> return index

compile :: Program a -> Compiler [Instr]
compile (Program stmts _) = do
    let numSlots = numVars stmts
    let setup = [ISub (Reg RSP) (Const (fromIntegral (wordSize * numSlots)))]
    bodyInstrs <- concat <$> mapM compileStatement stmts
    let cleanup = [IAdd (Reg RSP) (Const (fromIntegral (wordSize * numSlots))), IRet]
    return $ concat [setup, bodyInstrs, cleanup]

compileStatement :: Statement a -> Compiler [Instr]
compileStatement = \case
    Assign x rhs _ -> do
        index <- allocVar x
        rhsInstrs <- compileExpr rhs
        let bindInstrs = [IMov (RegOffset RBP (-index)) (Reg RAX)]
        return (rhsInstrs++bindInstrs)
    Return e _ -> compileExpr e -- TODO when you have functions, jump to the cleanup label here!

compileExpr :: Expr a -> Compiler [Instr]
compileExpr = \case
    EVar x _ -> do
        index <- lookupVar x
        return [IMov (Reg RAX) (RegOffset RBP (-index))]
    EInt n _ -> return [IMov (Reg RAX) (Const n)]

executeCompiler :: Compiler a -> Either String a
executeCompiler m = evalState (runExceptT (runCompiler m)) emptyEnv

compileStr :: Program a -> Either String String
compileStr p =
    let mInstrs = executeCompiler $ compile p in
    let pStrM = show <$> mInstrs in
    do
        pStr <- pStrM
        return $ intercalate "\n"
                [ "section .text"
                , "global our_code_starts_here"
                , "our_code_starts_here:"
                , pStr
                ]