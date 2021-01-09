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
    e <- pGetIndex
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

pIfElse :: Parser Statement
pIfElse = do
    pKeyword "if"
    cnd <- parens pExpr
    let go = braces (many pStatement) <|> ((:[]) <$> pStatement)
    thn <- go
    mEls <- optional (pKeyword "else" >> go)
    return $ If cnd thn mEls

pSAtomic :: Parser Statement
pSAtomic = choice [pReturn, try pDecl, try pDef, pAssign] <?> "statement"

pStatement :: Parser Statement
pStatement = pIfElse <|> pSAtomic

pProgram :: Parser Program
pProgram = Program <$> some pStatement

left :: (t -> a) -> Either t b -> Either a b
left f m = case m of
    Left l -> Left (f l)
    Right r -> Right r

-- | name then contents
parseProgram :: String -> String -> Either String Program
parseProgram name src = left errorBundlePretty $ runParser pProgram name src

parseExpr :: String -> String -> Either String Expr
parseExpr name src = left errorBundlePretty $ runParser pExpr name src
