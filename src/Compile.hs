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
import Type

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

-- | allocate an array of a given length and return the array pointer and the address of the first value.
-- Does not zero out words
allocArr :: String -> Integer -> Compiler (Arg, Arg)
allocArr x n = do
    (m, nextIndex) <- get
    let ptrAddr = RegOffset RBP (-nextIndex)
    let firstAddr = RegOffset RBP (-(succ nextIndex))
    put (Map.insert x ptrAddr m, 1 + n + nextIndex)
    return (ptrAddr, firstAddr)


compile :: Program -> Compiler [Instr]
compile (Program stmts) = do
    let slots = numSlots stmts + 1
    let setup = [ IPush (Reg RBP)
                , IMov (Reg RBP) (Reg RSP)
                , ISub (Reg RSP) (Const (fromIntegral (wordSize * slots)))
                ]
    bodyInstrs <- concat <$> mapM compileStatement stmts
    let cleanup = [ IAdd (Reg RSP) (Const (fromIntegral (wordSize * slots)))
                  , IMov (Reg RSP) (Reg RBP)
                  , IPop (Reg RBP)
                  , IRet
                  ]
    return $ concat [setup, bodyInstrs, cleanup]

-- | puts &arr[idx] in rax
compileGetIndex :: Expr -> Expr -> Compiler [Instr]
compileGetIndex arr idx = do
    arrPtrInstrs <- compileExpr arr
    idxInstrs <- compileExpr idx
    return $
        idxInstrs
        ++ [IPush (Reg RAX)]
        ++ arrPtrInstrs
        ++ [ IPop (Reg RCX)
           , IMul (Reg RCX) (Const wordSize)
           , ISub (Reg RAX) (Reg RCX)
           ]

-- | puts the location of the LHS inside rax (where to store the rhs)
compileLHS :: LHS -> Compiler [Instr]
compileLHS (LVar x) = do
    addr <- allocVar x
    return [ILea (Reg RAX) addr]
compileLHS (LDeref lhs) = do
    instrs <- compileLHS lhs
    return (instrs++[IMov (Reg RAX) (RegOffset RAX 0)])
compileLHS (LSetIndex arr idx) = compileGetIndex arr idx

-- | put the rhs in rax after assigning
compileAssignment :: LHS -> Expr -> Compiler [Instr]
compileAssignment lhs rhs = do
    lhsInstrs <- compileLHS lhs
    let saveLHS = [IPush (Reg RAX)]
    rhsInstrs <- compileExpr rhs
    let bindInstrs = [ IPop (Reg RCX), IMov (RegOffset RCX 0) (Reg RAX)]
    return (concat [lhsInstrs,saveLHS,rhsInstrs,bindInstrs])

imap :: (Integer -> b -> c) -> [b] -> [c]
imap = flip zipWith [0..]

-- | int[] xs = {1,2,3}; -> int[3] xs; xs[0] = 1; xs[1] = 2; xs[2] = 3;
-- puts &xs[0] in RAX
compileArrayLitDef :: Type -> Integer -> String -> [Expr] -> Compiler [Instr]
compileArrayLitDef t n x es = do
    let decl = Decl (TArray t (Just n)) x
        assignments = imap (\i e -> Assign (LSetIndex (EVar x) (EInt i)) e) es
    normalInstrs <- concat <$> mapM compileStatement (decl:assignments)
    ptrInstrs <- compileExpr (EVar x) -- return the pointer to arr[0]
    return $ normalInstrs ++ ptrInstrs

compileStatement :: Statement -> Compiler [Instr]
compileStatement = \case
    Assign lhs rhs -> compileAssignment lhs rhs
    Return e -> compileExpr e -- TODO when you have functions, jump to the cleanup label here!
    Decl (TArray _ (Just n)) x -> do
        (ptrAddr, firstAddr) <- allocArr x n
        let bindInstrs = [ILea (Reg RCX) firstAddr, IMov ptrAddr (Reg RCX)] -- store the address of the beginning of the array
        case firstAddr of
            RegOffset r i0 ->
                let zeroInstrs = [IMov (RegOffset r (i0-i)) (Const 0) | i <- [0..pred n]]
                in return (bindInstrs ++ zeroInstrs ++ [ILea (Reg RAX) firstAddr])
            _ -> throwError "non-regoffset location of array's first word"
    Decl _ x -> compileAssignment (LVar x) (EInt 0)
    Def t@(TArray _ (Just n)) x (EArrayLiteral es) -> compileArrayLitDef t n' x es
        where n' = max n (fromIntegral $ length es)
    Def t@(TArray _ Nothing) x (EArrayLiteral es) -> compileArrayLitDef t n x es
        where n = fromIntegral $ length es
    -- note that int[10] xs = ys; will not allocate stack/copy TODO write test and investigate gcc behavior
    Def _ x rhs -> compileAssignment (LVar x) rhs

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
    EUnop AddrOf e -> maybe (throwError "invalid addr of") compileLHS (lhsOfExpr e)
--    EUnop AddrOf (EVar x) -> do
--        addr <- lookupVar x
--        case addr of
--            RegOffset{} -> do
--                return [ILea (Reg RAX) addr]
--            _ -> throwError "address must be regoffset"
--    EUnop AddrOf _ -> throwError "invalid addr of"
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
    EGetIndex arr idx -> do
        ptrInstrs <- compileGetIndex arr idx -- &arr[idx]
        return $ ptrInstrs ++ [IMov (Reg RAX) (RegOffset RAX 0)] -- *&arr[idx]
    EArrayLiteral{} -> throwError "unexpected array literal"


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