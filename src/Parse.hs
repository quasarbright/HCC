module Parse where
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import ParseUtils
import AST

type ExprParser = (Parser() -> Parser (Expr SS)) -> Parser () -> Parser (Expr SS)

pNumber :: Parser (Expr SS)
pNumber = wrapSSWith (uncurry EInt) (lexeme L.decimal)

pVar :: Parser (Expr SS)
pVar = wrapSSM $ EVar <$> identifier


binary :: String -> Binop -> Operator Parser (Expr SS)
binary name op = InfixL $ do
    symbol name
    return $ \l r -> let ss = combineSS (getTag l) (getTag r) in EBinop op l r ss

prefix :: String -> Unop -> Operator Parser (Expr SS)
prefix  name op = Prefix $ do
    (_,ss) <- wrapSS $ symbol name
    return $ \e -> let ss' = combineSS ss (getTag e) in EUnop op e ss'

postfix :: String -> Unop -> Operator Parser (Expr SS)
postfix  name op = Prefix $ do
    (_,ss) <- wrapSS $ symbol name
    return $ \e -> let ss' = combineSS (getTag e) ss in EUnop op e ss'

pAtomic :: Parser (Expr SS)
pAtomic = choice [pVar, pNumber, between (symbol "(") (symbol ")") pExpr]

pExpr :: Parser (Expr SS)
pExpr = makeExprParser pAtomic table

table :: [[Operator Parser (Expr SS)]]
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

pAssign :: Parser (Statement SS)
pAssign = wrapSSM $ Assign <$> (identifier <* pReservedOp "=") <*> (pExpr <* pReservedOp ";")

pReturn :: Parser (Statement SS)
pReturn = wrapSSM $ Return <$> (pKeyword "return" *> pExpr <* pReservedOp ";")

pStatement :: Parser (Statement SS)
pStatement = choice [pReturn, pAssign]

pProgram :: Parser (Program SS)
pProgram = wrapSSM $ Program <$> some pStatement

-- | name then contents
parseProgram :: String -> String -> Either (ParseErrorBundle String Void) (Program SS)
parseProgram = runParser pProgram

parseExpr :: String -> String -> Either (ParseErrorBundle String Void) (Expr SS)
parseExpr = runParser pExpr
