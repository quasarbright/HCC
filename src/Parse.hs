module Parse where
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import ParseUtils
import AST

type ExprParser = (Parser() -> Parser (Expr SS)) -> Parser () -> Parser (Expr SS)

pNumber :: Parser (Expr SS)
pNumber = wrapSSWith (uncurry EInt) $ L.signed scn L.decimal

pVar :: Parser (Expr SS)
pVar = wrapSSM $ EVar <$> identifier

pExpr :: Parser (Expr SS)
pExpr = choice [pVar, pNumber]
        
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
