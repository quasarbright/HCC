module Parse where
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import ParseUtils
import AST
import Type
import Data.Functor (($>))

pTInt :: Parser Type
pTInt = symbol "int" $> TInt

pTRef :: Parser Type
pTRef = do
    t <- pTInt
    stars <- many (symbol "*")
    return (foldr (const TRef) t stars)

pType :: Parser Type
pType = pTRef <?> "type"

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

pAtomic :: Parser Expr
pAtomic = choice [pVar, pNumber, between (symbol "(") (symbol ")") pExpr]

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
    e <- pAtomic
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
    stars <- many (symbol "*")
    name <- identifier
    return $ foldr (const LDeref) (LVar name) stars

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

pStatement :: Parser Statement
pStatement = choice [pReturn, try pDecl, try pDef, pAssign] <?> "statement"

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
