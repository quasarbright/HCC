module Parse where
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import ParseUtils
import AST

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

pExpr :: Parser Expr
pExpr = makeExprParser pAtomic table

table :: [[Operator Parser Expr]]
table = [ [ prefix  "-" Neg
          , prefix "~" Inv
          , prefix "!" Not
          , prefix "*" Deref
          , prefix "&" AddrOf
          ]
        , [binary "*" Times]
        , [binary "+" Plus]
        , [binary "==" Eq]
        , [binary "|" BitOr]
        ]

pAssign :: Parser Statement
pAssign = Assign <$> (identifier <* pReservedOp "=") <*> (pExpr <* pReservedOp ";")

pReturn :: Parser Statement
pReturn = Return <$> (pKeyword "return" *> pExpr <* pReservedOp ";")

pStatement :: Parser Statement
pStatement = choice [pReturn, pAssign]

pProgram :: Parser Program
pProgram = Program <$> some pStatement

-- | name then contents
parseProgram :: String -> String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram

parseExpr :: String -> String -> Either (ParseErrorBundle String Void) Expr
parseExpr = runParser pExpr
