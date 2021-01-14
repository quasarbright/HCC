module Parse where
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import ParseUtils
import AST
import Type
import Data.Functor (($>))

pTAtomic :: Parser Type
pTAtomic = choice
               [ symbol "int" $> TInt
               , symbol "void" $> TVoid
               ]

pTRef_ :: Parser (Type -> Type)
pTRef_ = symbol "*" $> TRef

pTArray_ :: Parser (Type -> Type)
pTArray_ = do
    mn <- brackets (optional (lexeme L.decimal))
    return $ \t -> TArray t mn

pTOp :: Parser Type
pTOp = do
    t <- pTAtomic -- TODO void
    ops <- reverse <$> many (choice [pTRef_, pTArray_]) -- reverse for foldr
    return $ foldr ($) t ops

pType :: Parser Type
pType = pTOp

type ExprParser = (Parser() -> Parser Expr) -> Parser () -> Parser Expr

pNumber :: Parser Expr
pNumber = EInt <$> lexeme L.decimal

pVar :: Parser Expr
pVar = EVar <$> identifier


binary :: String -> Binop -> Operator Parser Expr
binary name op = InfixL $ do
    symbol name
    return $ \l r -> EBinop op l r

prefix :: String -> Unop -> Operator Parser Expr
prefix  name op = Prefix $ do
    symbol name
    return $ \e -> EUnop op e

postfix :: String -> Unop -> Operator Parser Expr
postfix  name op = Prefix $ do
    symbol name
    return $ \e -> EUnop op e

pArrayLiteral :: Parser Expr
pArrayLiteral = EArrayLiteral <$> braces (pExpr `sepBy` symbol ",")

pAtomic :: Parser Expr
pAtomic = choice [pVar, pNumber, parens pExpr, pArrayLiteral]

pGetIndex :: Parser Expr
pGetIndex = do
    e <- pAtomic
    mIdx <- optional (brackets pExpr)
    case mIdx of
        Just idx -> return $ EGetIndex e idx
        Nothing -> return e

pApp :: Parser Expr
pApp = do
    -- TODO arbitrary expressions if you do function pointers
    f <- EVar <$> identifier
    args <- parens (pExpr `sepBy` symbol ",")
    return $ EApp f args

pAppGet :: Parser Expr
pAppGet = try pApp <|> pGetIndex

pUnop_ :: Parser (Expr -> Expr)
pUnop_ = EUnop <$> choice
    [ symbol "*" $> Deref
    , symbol "&" $> AddrOf
    , symbol "-" $> Neg
    , symbol "!" $> Not
    , symbol "~" $> Inv
    ]

pUnop :: Parser Expr
pUnop = do
    ops <- many pUnop_
    e <- pAppGet
    return (foldr ($) e ops)

--pDeref = do
--    stars <- many (symbol "*")
--    stars <- many (symbol "*")

pExpr :: Parser Expr
pExpr = makeExprParser pUnop table <?> "expression"

table :: [[Operator Parser Expr]]
table = [ [binary "*" Times]
        , [binary "+" Plus]
        , [binary "==" Eq]
        , [binary "&" BitAnd]
        , [binary "|" BitOr]
        ]

pLHS :: Parser LHS
pLHS = do
    e <- pExpr
    maybe (fail "invalid lhs") return (lhsOfExpr e)

pAssign :: Parser Statement
pAssign = Assign <$> (pLHS <* pReservedOp "=") <*> (pExpr <* pReservedOp ";")

pReturn :: Parser Statement
pReturn = Return <$> (pKeyword "return" *> pExpr <* pReservedOp ";")

pDecl :: Parser Statement
pDecl = do
    t <- pType
    x <- identifier
    pReservedOp ";"
    return (Decl t x)

pDef :: Parser Statement
pDef = do
    t <- pType
    x <- identifier
    pReservedOp "="
    rhs <- pExpr
    pReservedOp ";"
    return (Def t x rhs)

pBlock :: Parser [Statement]
pBlock = braces (many pStatement)

pSingleOrBlock :: Parser [Statement]
pSingleOrBlock = pBlock <|> ((:[]) <$> pStatement)

pIfElse :: Parser Statement
pIfElse = do
    pKeyword "if"
    cnd <- parens pExpr
    thn <- pSingleOrBlock
    mEls <- optional (pKeyword "else" >> pSingleOrBlock)
    return $ If cnd thn mEls

pWhile :: Parser Statement
pWhile = pKeyword "while" *> (While <$> parens pExpr <*> pSingleOrBlock)

pFor :: Parser Statement
pFor = do
    pKeyword "for"
    symbol "("
    setup <- pSAtomic
    cnd <- pExpr
    symbol ";"
    update <- pSAtomic
    symbol ")"
    For setup cnd update <$> pSingleOrBlock

pSAtomic :: Parser Statement
pSAtomic = choice [pReturn, try pDecl, try pDef, pAssign] <?> "statement"

pStatement :: Parser Statement
pStatement = choice [pIfElse, pWhile, pFor, pSAtomic]

pFunDefOrDecl :: Parser TopDecl
pFunDefOrDecl = do
    tRet <- pType
    f <- identifier
    targs <- parens (((,) <$> pType <*> identifier) `sepBy` symbol ",")
    let decl = symbol ";" $> FunDecl tRet f targs
    let def = FunDef tRet f targs <$> pBlock
    decl <|> def

pTopDecl :: Parser TopDecl
pTopDecl = pFunDefOrDecl

pProgram :: Parser Program
pProgram = Program <$> some pFunDefOrDecl

left :: (t -> a) -> Either t b -> Either a b
left f m = case m of
    Left l -> Left (f l)
    Right r -> Right r


makeParseFn :: Parser a -> String -> String -> Either String a
makeParseFn p name src = left errorBundlePretty $ runParser (scn *> p <* eof) name src

parseBlock :: String -> String -> Either String [Statement]
parseBlock = makeParseFn pBlock

-- | name then contents
parseProgram :: String -> String -> Either String Program
parseProgram = makeParseFn pProgram

parseExpr :: String -> String -> Either String Expr
parseExpr = makeParseFn pExpr
