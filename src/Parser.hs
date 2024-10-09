module Parser (
    Parser,
    expr,
    lcType,
    abstraction,
    application,
    ifExpr,
    var,
    bool
) where

import Data.Void (Void)
import Data.Functor (($>))
import Text.Megaparsec ((<?>), (<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP (string, char, space, space1)
import ParseTree (ParseTree (Constant, Variable, IfExpr, Application, Abstraction), LCType (Boolean, (:->)), WithContext((:@)))
import Data.Char (isAlpha)

type Parser = MP.Parsec Void String

expr :: Parser (WithContext ParseTree)
expr = application <?> "expression"


expr' :: Parser (WithContext ParseTree)
expr' = abstraction 
    <|> ifExpr
    <|> bool
    <|> var
    <|> withParenthesis expr
    <?> "expression"


lcType :: Parser LCType
lcType = 
    do
        left <- boolType <|> withParenthesis lcType
        MP.space
        functionType left <|> return left 
    <?> "type annotation"
    where 
        functionType left = do
            _ <- MP.string "->"
            MP.space
            right <- lcType
            return $ left :-> right
        boolType = MP.string "Bool" $> Boolean

abstraction :: Parser (WithContext ParseTree)
abstraction = do
    pos <- MP.getOffset
    _ <- MP.char '\\'
    MP.space
    v <- var
    MP.space
    _ <- MP.char ':'
    MP.space
    typePos <- MP.getOffset
    t <- lcType
    MP.space
    _ <- MP.char '.'
    MP.space
    body <- expr
    return $ Abstraction v (t :@ typePos) body :@ pos
    <?> "abstraction"

application :: Parser (WithContext ParseTree)
application = do
    op@(_ :@ pos) <- expr'
    MP.space
    operands <- MP.many (expr' <* MP.space)
    return $ foldl (\ o v -> Application o v :@ pos) op operands
    <?> "application" 

ifExpr :: Parser (WithContext ParseTree)
ifExpr = do
    pos <- MP.getOffset
    _ <- MP.string "if"
    MP.space1
    cond <- expr'
    MP.space1
    _ <- MP.string "then"
    MP.space1
    onTrue <- expr'
    MP.space1
    _ <- MP.string "else"
    MP.space1
    onFalse <- expr'
    return $ IfExpr cond onTrue onFalse :@ pos
    <?> "if expression"

var :: Parser (WithContext ParseTree)
var = do
    pos <- MP.getOffset
    identifier <- MP.takeWhile1P (Just "alphabetic character") isAlpha
    return $ Variable identifier :@ pos
    <?> "variable"  

bool :: Parser (WithContext ParseTree)
bool = do
    pos <- MP.getOffset
    value <- MP.string "true" $> True <|> MP.string "false" $> False
    return $ Constant value :@ pos
    <?> "boolean"

withParenthesis :: Parser a -> Parser a
withParenthesis p = MP.char '(' *> MP.space *> p <* MP.space <* MP.char ')'