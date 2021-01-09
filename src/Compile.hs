{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Compile where

import AST
import Asm

import Data.List (intercalate)
import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Type

-- | maps names to stack indices
type Env = Map String Arg

-- | next available stack index, next available tag
type Store = (Integer, Integer)

emptyEnv :: Env
emptyEnv = Map.empty

emptyStore :: Store
emptyStore = (1,1)

newtype Compiler a = Compiler { runCompiler :: ExceptT String (RWS Env () Store) a }
                 deriving( Functor
                         , Applicative
                         , Monad
                         , MonadReader Env
                         , MonadState Store
                         , MonadError String
                         )

lookupVar :: String -> Compiler Arg
lookupVar x = do
    m <- ask
    case Map.lookup x m of
        Nothing -> throwError "unbound variable"
        Just arg -> return arg

-- | allocate a stack slot for a new variable.
-- NOTE: does not modify mapping from names to addrs.
allocVar :: Compiler Arg
allocVar = do
    (nextIndex,nextTag) <- get
    let addr = RegOffset RBP (-nextIndex)
    put (succ nextIndex, nextTag)
    return addr

-- | run the compiler with a given variable in scope
withVar :: String -> Arg -> Compiler a -> Compiler a
withVar x addr = local (Map.insert x addr)

-- | run the compiler with given variables in scope (adds them)
withVars :: [(String, Arg)] -> Compiler a -> Compiler a
withVars vars m = foldr (uncurry withVar) m vars

-- | allocate an array of a given length and return the array pointer and the address of the first value.
-- Does not zero out words.
-- Does not modify mapping from names to addrs
allocArr :: Integer -> Compiler (Arg, Arg)
allocArr n = do
    (nextIndex, nextTag) <- get
    let ptrAddr = RegOffset RBP (-nextIndex)
    let firstAddr = RegOffset RBP (-(succ nextIndex))
    put (1 + n + nextIndex, nextTag)
    return (ptrAddr, firstAddr)

getTag :: Compiler Integer
getTag = do
    (i,tag) <- get
    put (i,succ tag)
    return tag

compile :: Program -> Compiler [Instr]
compile (Program stmts) = do
    let slots = numSlots stmts + 1
    let setup = [ IPush (Reg RBP)
                , IMov (Reg RBP) (Reg RSP)
                , ISub (Reg RSP) (Const (fromIntegral (wordSize * slots)))
                ]
    bodyInstrs <- compileBlock stmts
    let cleanup = [ IAdd (Reg RSP) (Const (fromIntegral (wordSize * slots)))
                  , IMov (Reg RSP) (Reg RBP)
                  , IPop (Reg RBP)
                  , IRet
                  ]
    return $ concat [setup, bodyInstrs, cleanup]

--compileBlock :: [Statement] -> Compiler [Instr]
--compileBlock = fmap concat . mapM compileStatement

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
    addr <- lookupVar x
    return [ILea (Reg RAX) addr]
compileLHS (LDeref lhs) = do
    instrs <- compileLHS lhs
    return (instrs++[IMov (Reg RAX) (RegOffset RAX 0)])
compileLHS (LSetIndex arr idx) = compileGetIndex arr idx

-- | Does not allocate space!
-- Puts the rhs in rax after assigning
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
compileArrayLitDef :: Type -> Integer -> String -> [Expr] -> [Statement] -> Compiler [Instr]
compileArrayLitDef t n x es rest =
    let decl = Decl (TArray t (Just n)) x
        assignments = imap (\i e -> Assign (LSetIndex (EVar x) (EInt i)) e) es
    in compileBlock ((decl:assignments)++rest)

infixl 1 >>++
(>>++) :: Applicative f => f [a] -> f [a] -> f [a]
m1 >>++ m2 = (++) <$> m1 <*> m2

-- | Compile a block of statements.
-- Can be used for function body or if/loop body
compileBlock :: [Statement] -> Compiler [Instr]
compileBlock [] = return []
compileBlock (stmt:rest) =
    let mRest = compileBlock rest in
    case stmt of
        Assign lhs rhs -> compileAssignment lhs rhs >>++ mRest
        Return e -> compileExpr e >>++ mRest -- TODO when you have functions, jump to the cleanup label here!
        Decl (TArray _ (Just n)) x -> do
            (ptrAddr, firstAddr) <- allocArr n
            let bindInstrs = [ILea (Reg RCX) firstAddr, IMov ptrAddr (Reg RCX)] -- store the address of the beginning of the array
            case firstAddr of
                RegOffset r i0 ->
                    let zeroInstrs = [IMov (RegOffset r (i0-i)) (Const 0) | i <- [0..pred n]]
                        instrs = bindInstrs ++ zeroInstrs ++ [ILea (Reg RAX) firstAddr]
                    in return instrs >>++ withVar x ptrAddr mRest
                _ -> throwError "non-regoffset location of array's first word"
        Decl _ x -> do
            addr <- allocVar
            withVar x addr (compileAssignment (LVar x) (EInt 0) >>++ mRest)
        Def t@(TArray _ (Just n)) x (EArrayLiteral es) -> compileArrayLitDef t n' x es rest
            where n' = max n (fromIntegral $ length es)
        Def t@(TArray _ Nothing) x (EArrayLiteral es) -> compileArrayLitDef t n x es rest
            where n = fromIntegral $ length es
        -- note that int[10] xs = ys; will not allocate stack/copy TODO write test and investigate gcc behavior
        Def _ x rhs -> do
            addr <- allocVar
            withVar x addr (compileAssignment (LVar x) rhs >>++ mRest)
        If cnd thn mEls -> do
            t <- getTag
            cndInstrs <- compileExpr cnd
            thnInstrs <- compileBlock thn
            elsInstrs <- maybe (return []) compileBlock mEls
            let else_label = "else_"++show t
                done_label = "endif_"++show t
            let instrs = concat
                    [ [IComment "check if condition"]
                    , cndInstrs
                    , [ IComment "jump to else if false"
                      , ICmp (Reg RAX) (Const 0)
                      , IJe else_label
                      ]
                    , thnInstrs
                    , [ IComment "skip past else"
                      , IJmp done_label
                      , ILabel else_label
                      ]
                    , elsInstrs
                    , [ILabel done_label]
                    ]
            return instrs >>++ mRest
        While cnd body -> do
            t <- getTag
            cndInstrs <- compileExpr cnd
            bodyInstrs <- compileBlock body
            let loop_label = "while_"++show t
                done_label = "endwhile_"++show t
            let instrs = concat
                    [ [ILabel loop_label]
                    , cndInstrs
                    , [ IComment "check loop condition"
                      , ICmp (Reg RAX) (Const 0)
                      , IJe done_label
                      ]
                    , bodyInstrs
                    , [ IComment "loop"
                      , IJmp loop_label
                      , ILabel done_label
                      ]
                    ]
            return instrs >>++ mRest
        For setup cnd update body -> compileBlock (whileOfFor setup cnd update body) >>++ mRest
       

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
executeCompiler m = fst $ evalRWS (runExceptT (runCompiler m)) emptyEnv emptyStore

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