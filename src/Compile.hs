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
type Env = (Map String Arg, Integer)

emptyEnv :: Env
emptyEnv = (Map.empty, 1)

newtype Compiler a = Compiler { runCompiler :: ExceptT String (State Env) a }
    deriving( Functor
            , Applicative
            , Monad
            , MonadState Env
            , MonadError String
            )

lookupVar :: String -> Compiler Arg
lookupVar x = do
    (m,_) <- get
    case Map.lookup x m of
        Nothing -> throwError "unbound variable"
        Just arg -> return arg

-- | allocate a stack slot for the new variable if we haven't seen it. Otherwise, return its slot
allocVar :: String -> Compiler Arg
allocVar x = do
    (m,nextIndex) <- get
    case Map.lookup x m of
        Nothing -> do
            let addr = RegOffset RBP (-nextIndex)
            put (Map.insert x addr m, succ nextIndex)
            return addr
        Just arg -> return arg

compile :: Program -> Compiler [Instr]
compile (Program stmts) = do
    let numSlots = numVars stmts
    let setup = [ISub (Reg RSP) (Const (fromIntegral (wordSize * numSlots)))]
    bodyInstrs <- concat <$> mapM compileStatement stmts
    let cleanup = [IAdd (Reg RSP) (Const (fromIntegral (wordSize * numSlots))), IRet]
    return $ concat [setup, bodyInstrs, cleanup]

compileStatement :: Statement -> Compiler [Instr]
compileStatement = \case
    Assign x rhs -> do
        addr <- allocVar x
        rhsInstrs <- compileExpr rhs
        let bindInstrs = [IMov addr (Reg RAX)]
        return (rhsInstrs++bindInstrs)
    Return e -> compileExpr e -- TODO when you have functions, jump to the cleanup label here!

simpleBinop :: (Arg -> Arg -> Instr) -> Expr -> Expr -> Compiler [Instr]
simpleBinop instr left right = do
        leftInstrs <- compileExpr left
        rightInstrs <- compileExpr right
        return . concat $
               [ leftInstrs
               , [IPush (Reg RAX)]
               , rightInstrs
               , [IPop (Reg RCX), instr (Reg RCX) (Reg RAX), IMov (Reg RAX) (Reg RCX)]
               ]

compileExpr :: Expr -> Compiler [Instr]
compileExpr = \case
    EVar x -> do
        addr <- lookupVar x
        return [IMov (Reg RAX) addr]
    EUnop AddrOf (EVar x) -> do
        addr <- lookupVar x
        case addr of
            RegOffset reg off -> do
                return [IMov (Reg RAX) (Reg reg), IAdd (Reg RAX) (Const (off * wordSize))]
            _ -> throwError "address must be regoffset"
    EUnop AddrOf _ -> throwError "invalid addr of"
    EUnop Deref e -> do
        ptrInstrs <- compileExpr e
        return $ ptrInstrs ++ [IMov (Reg RAX) (RegOffset RAX 0)]
    EUnop Inv e -> do
        eInstrs <- compileExpr e
        return $ eInstrs ++ [INot (Reg RAX)]
    EUnop Not _ -> undefined -- TODO labels and jumps
    EUnop Neg e -> do
        eInstrs <- compileExpr e
        return $ eInstrs ++ [INeg (Reg RAX)]
    EBinop Plus left right -> simpleBinop IAdd left right
    EBinop Times left right -> simpleBinop IMul left right
    EBinop BitAnd left right -> simpleBinop IAnd left right
    EBinop BitOr left right -> simpleBinop IOr left right
    EBinop Eq _ _ -> undefined -- TODO labels and jumps
    EInt n -> return [IMov (Reg RAX) (Const n)]

executeCompiler :: Compiler a -> Either String a
executeCompiler m = evalState (runExceptT (runCompiler m)) emptyEnv

compileStr :: Program -> Either String String
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