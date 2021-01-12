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
import Data.Function ((&))

-- | maps names to stack indices
type Env = Map String Arg

-- | next available stack index, next available tag
type Store = (Integer, Integer)

emptyEnv :: Env
emptyEnv = Map.empty

emptyStore :: Store
emptyStore = (1,1)

newtype Compiler a = Compiler { runCompiler :: ExceptT String (RWS Env [Instr] Store) a }
                 deriving( Functor
                         , Applicative
                         , Monad
                         , MonadReader Env
                         , MonadState Store
                         , MonadWriter [Instr]
                         , MonadError String
                         )

-- utility --

nothing :: Monad m => m ()
nothing = return ()

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

modifyStackIndex :: (Integer -> Integer) -> Compiler ()
modifyStackIndex f = do
    (nextIndex, nextTag) <- get
    put (f nextIndex, nextTag)

getTag :: Compiler Integer
getTag = do
    (i,tag) <- get
    put (i,succ tag)
    return tag

-- compilation --

{-
Invariants:
- RSP is at the same place before and after compiling a statement/expression (every push has a pop)
- the nextStackIndex of the state points to the next stack index available for a variable to be placed in
- tags (for label generation) are unique (increment the nextTag when you generate one)
- the environment represents variables that are currently in scope for the code being compiled
-}

-- | puts &arr[idx] in rax
compileGetIndex :: Expr -> Expr -> Compiler ()
compileGetIndex arr idx = do
    compileExpr idx
    tell [IPush (Reg RAX)]
    compileExpr arr
    tell [ IPop (Reg RCX)
         , IMul (Reg RCX) (Const wordSize)
         , ISub (Reg RAX) (Reg RCX)
         ]

-- | puts the location of the LHS inside rax (where to store the rhs)
compileLHS :: LHS -> Compiler ()
compileLHS (LVar x) = do
    addr <- lookupVar x
    tell [ILea (Reg RAX) addr]
compileLHS (LDeref lhs) = do
    compileLHS lhs
    tell [IMov (Reg RAX) (RegOffset RAX 0)]
compileLHS (LSetIndex arr idx) = compileGetIndex arr idx

-- | Does not allocate space!
-- Puts the rhs in rax after assigning
compileAssignment :: LHS -> Expr -> Compiler ()
compileAssignment lhs rhs = do
    compileLHS lhs
    tell [IPush (Reg RAX)]
    compileExpr rhs
    tell [IPop (Reg RCX), IMov (RegOffset RCX 0) (Reg RAX)]

imap :: (Integer -> b -> c) -> [b] -> [c]
imap = flip zipWith [0..]

-- | int[] xs = {1,2,3}; -> int[3] xs; xs[0] = 1; xs[1] = 2; xs[2] = 3;
-- puts &xs[0] in RAX
desugarArrayLitDef :: Type -> Integer -> String -> [Expr] -> [Statement]
desugarArrayLitDef t n x es =
    let decl = Decl (TArray t (Just n)) x
        assignments = imap (Assign . LSetIndex (EVar x) . EInt) es -- xs[i] = {es !! i};
    in (decl:assignments)

-- | Compile a block of statements.
-- Can be used for function body or if/loop body
compileBlock :: String -> [Statement] -> Compiler ()
compileBlock _ [] = return ()
compileBlock retLabel (stmt:rest) =
    let recurse = compileBlock retLabel
        mRest = recurse rest in
    case stmt of
        Assign lhs rhs -> compileAssignment lhs rhs >> mRest
        Return e -> compileExpr e >> tell [IJmp retLabel]
        Decl (TArray _ (Just n)) x -> do
            (ptrAddr, firstAddr) <- allocArr n
            let bindInstrs = [ILea (Reg RCX) firstAddr, IMov ptrAddr (Reg RCX)] -- store the address of the beginning of the array
            case firstAddr of
                RegOffset r i0 ->
                    let zeroInstrs = [IMov (RegOffset r (i0-i)) (Const 0) | i <- [0..pred n]]
                        instrs = bindInstrs ++ zeroInstrs ++ [ILea (Reg RAX) firstAddr]
                    in tell instrs >> withVar x ptrAddr mRest
                _ -> throwError "non-regoffset location of array's first word"
        Decl _ x -> do
            addr <- allocVar
            withVar x addr (compileAssignment (LVar x) (EInt 0) >> mRest)
        Def t@(TArray _ (Just n)) x (EArrayLiteral es) -> recurse (desugarArrayLitDef t n' x es++rest)
            where n' = max n (fromIntegral $ length es)
        Def t@(TArray _ Nothing) x (EArrayLiteral es) -> recurse (desugarArrayLitDef t n x es++rest)
            where n = fromIntegral $ length es
        Def _ x rhs -> do
            addr <- allocVar
            withVar x addr (compileAssignment (LVar x) rhs >> mRest)
        If cnd thn mEls -> do
            t <- getTag
            let else_label = "else_"++show t
                done_label = "endif_"++show t
            tell [IComment "check if condition"]
            compileExpr cnd
            tell [ IComment "jump to else if false"
                 , ICmp (Reg RAX) (Const 0)
                 , IJe else_label
                 ]
            recurse thn
            tell [ IComment "skip past else"
                 , IJmp done_label
                 , ILabel else_label
                 ]
            maybe (return ()) recurse mEls
            tell [ILabel done_label]
            mRest
        While cnd body -> do
            t <- getTag
            let loop_label = "while_"++show t
                done_label = "endwhile_"++show t
            tell [ILabel loop_label]
            compileExpr cnd
            tell [ IComment "check loop condition"
                 , ICmp (Reg RAX) (Const 0)
                 , IJe done_label
                 ]
            recurse body
            tell [ IComment "loop"
                 , IJmp loop_label
                 , ILabel done_label
                 ]
            mRest
        For setup cnd update body -> recurse (whileOfFor setup cnd update body) >> mRest


simpleBinop :: (Arg -> Arg -> Instr) -> Expr -> Expr -> Compiler ()
simpleBinop instr left right = do
        compileExpr left
        tell [IPush (Reg RAX)]
        compileExpr right
        tell [IPop (Reg RCX), instr (Reg RCX) (Reg RAX), IMov (Reg RAX) (Reg RCX)]

loadArgsForCall :: [Expr] -> Compiler ()
loadArgsForCall args
    | length args <= 6 =
        let go e reg = compileExpr e >> tell [IMov (Reg reg) (Reg RAX)]
        in zipWithM_ go args argRegisters
    | otherwise =
        let go e = compileExpr e >> tell [IPush (Reg RAX)]
        in loadArgsForCall (take 6 args) >> mapM_ go (args & drop 6 & reverse)

compileExpr :: Expr -> Compiler ()
compileExpr = \case
    EVar x -> do
        addr <- lookupVar x
        tell [IMov (Reg RAX) addr]
    EUnop AddrOf e -> maybe (throwError "invalid addr of") compileLHS (lhsOfExpr e)
--    EUnop AddrOf (EVar x) -> do
--        addr <- lookupVar x
--        case addr of
--            RegOffset{} -> do
--                return [ILea (Reg RAX) addr]
--            _ -> throwError "address must be regoffset"
--    EUnop AddrOf _ -> throwError "invalid addr of"
    EUnop Deref e -> do
        compileExpr e
        tell [IMov (Reg RAX) (RegOffset RAX 0)]
    EUnop Inv e -> do
        compileExpr e
        tell [INot (Reg RAX)]
    EUnop Not e -> do
        compileExpr e
        t <- getTag
        let false_label = "not_false_"++show t
            done_label = "endnot_"++show t
        tell
            [ IComment "not"
            , ICmp (Reg RAX) (Const 0)
            , IJe false_label
            , IMov (Reg RAX) (Const 0)
            , IJmp done_label
            , ILabel false_label
            , IMov (Reg RAX) (Const 1)
            , ILabel done_label
            ]
    EUnop Neg e -> do
        compileExpr e
        tell [INeg (Reg RAX)]
    EBinop Plus left right -> simpleBinop IAdd left right
    EBinop Times left right -> simpleBinop IMul left right
    EBinop BitAnd left right -> simpleBinop IAnd left right
    EBinop BitOr left right -> simpleBinop IOr left right
    EBinop Eq l r -> do
        compileExpr l
        tell [IAnnot (IPush (Reg RAX)) "save left side of =="]
        compileExpr r
        t <- getTag
        let eq_label = "eq_"++show t
            done_label = "endeq_"++show t
        tell
            [ IAnnot (IPop (Reg RCX)) "restore left side of == to rcx"
            , IComment "comparison for =="
            , ICmp (Reg RAX) (Reg RCX)
            , IJe eq_label
            , IMov (Reg RAX) (Const 0)
            , IJmp done_label
            , ILabel eq_label
            , IMov (Reg RAX) (Const 1)
            , ILabel done_label
            ]
    EInt n -> tell [IMov (Reg RAX) (Const n)]
    EGetIndex arr idx -> do
        compileGetIndex arr idx -- &arr[idx]
        tell [IMov (Reg RAX) (RegOffset RAX 0)] -- *&arr[idx]
    EArrayLiteral{} -> throwError "unexpected array literal"
    EApp (EVar f) args -> do
        loadArgsForCall args
        tell [ICall (callLabel f)]

        when (length args > length argRegisters)
            (tell [IAdd (Reg RSP) (Const (fromIntegral $ wordSize * (length args - length argRegisters)))])
    EApp{} -> throwError "can't compile non-var calls"


-- variable mangling --
{-
- compiler labels are not underscored
- need to escape user stuff to remove ambiguity
- encoding assumes no shadowing of functions+globals (same namespace)
- must add something unused in compiler labels (like _) to beginning and something unique at the end
    to avoid possible ambiguity.
    - There must not be common suffixes in the encoding
-}
varToLabel :: [Char] -> [Char]
varToLabel f = "_"++f

callLabel :: [Char] -> [Char]
callLabel f = varToLabel f ++ "_call"

cleanupLabel :: [Char] -> [Char]
cleanupLabel f = varToLabel f ++ "_cleanup"

constLabel :: [Char] -> [Char]
constLabel x = varToLabel x ++ "_const"

-- | loads arguments into stack like locals (doesn't bind variables)
loadArgs :: Int -> Compiler ()
loadArgs n = tell . concat $ imap go (take n locs)
    where locs = fmap Reg argRegisters ++ [RegOffset RBP i | i <- [2..]]
          go i (Reg r) = [IMov (RegOffset RBP (-(i+1))) (Reg r)]
          go i arg = [IMov (Reg R10) arg, IMov (RegOffset RBP (-(i+1))) (Reg R10)]

compileDecls :: [TopDecl] -> Compiler ()
compileDecls [] = nothing
compileDecls (decl:rest) = let mRest = compileDecls rest in case decl of
    FunDecl{} -> mRest
    FunDef _ f targs body ->
        let arity = length targs
            slots = arity + numSlots body
        in do
            when (f == "main") (tell [ILabel "_main"])
            tell
                [ IComment $ "function "++f
                , ILabel (callLabel f)
                , IComment "setup"
                , IPush (Reg RBP)
                , IMov (Reg RBP) (Reg RSP)
                , ISub (Reg RSP) (Const (fromIntegral (wordSize * slots)))
                , IComment "load arguments from registers/stack"
                ]
            loadArgs arity
--            tell [IMov (RegOffset RBP (-(fromIntegral arity + 1))) (ALabel (callLabel f))]
            let vars = fmap snd targs
                locs = [RegOffset RBP (-i) | i <- [1..]]
                annots = zip vars locs
            modifyStackIndex (const (toInteger (succ arity)))
            tell [IComment "function body"]
            withVars annots $ compileBlock (cleanupLabel f) body
            tell
                [ IComment "cleanup"
                , ILabel (cleanupLabel f)
                , IAdd (Reg RSP) (Const (fromIntegral (wordSize * slots)))
                , IMov (Reg RSP) (Reg RBP)
                , IPop (Reg RBP)
                , IRet
                ]
            mRest

compile :: Program -> Compiler ()
compile (Program decls) = compileDecls decls

executeCompiler :: Compiler a -> Either String [Instr]
executeCompiler m = case evalRWS (runExceptT (runCompiler m)) emptyEnv emptyStore of
    (Left err,_) -> Left err
    (Right{},instrs) -> Right instrs

hasMain :: Program -> Bool
hasMain (Program decls) = any go decls
    where go FunDecl{} = False
          go (FunDef _ "main" _ _) = True
          go FunDef{} = False

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
                , if hasMain p then "    jmp "++callLabel "main" else "    mov rax, 0 ; no main\n    ret"
                , pStr
                ]
