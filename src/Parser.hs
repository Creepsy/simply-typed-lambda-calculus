module Parser (
    ErrorBundle,
    Parser,
    expr,
    lcType,
    abstraction,
    application,
    ifExpr,
    var,
    bool
) where

import Data.Functor (($>))
import Text.Megaparsec ((<?>), (<|>))
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MP (string, char, space, space1)
import ParseTree (LCType (Boolean, (:->)), Positioned (Positioned), ParseTree, ParseTree' (Application, Abstraction, IfExpr, Variable, Constant))
import Context((<@>))
import Data.Char (isAlpha)
import TypeError (LCError)

type ErrorBundle = MP.ParseErrorBundle String LCError
type Parser = MP.Parsec LCError String

expr :: Parser (ParseTree Positioned)
expr = application <?> "expression"


expr' :: Parser (ParseTree Positioned)
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

abstraction :: Parser (ParseTree Positioned)
abstraction = do
    pos <- MP.getOffset
    _ <- MP.char '\\'
    MP.space
    v <- ident
    MP.space
    _ <- MP.char ':'
    MP.space
    t <- lcType
    MP.space
    _ <- MP.char '.'
    MP.space
    body <- expr
    return $ Abstraction (v, t) body <@> pos
    <?> "abstraction"

application :: Parser (ParseTree Positioned)
application = do
    op@(Positioned _ pos) <- expr'
    MP.space
    operands <- MP.many (expr' <* MP.space)
    return $ foldl (\ o v -> Application o v <@> pos) op operands
    <?> "application" 

ifExpr :: Parser (ParseTree Positioned)
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
    return $ IfExpr cond onTrue onFalse <@> pos
    <?> "if expression"

var :: Parser (ParseTree Positioned)
var = do
    pos <- MP.getOffset
    identifier <- ident
    return $ Variable identifier <@> pos
    <?> "variable"  

ident :: Parser String
ident = MP.takeWhile1P (Just "alphabetic character") isAlpha <?> "identifier"

bool :: Parser (ParseTree Positioned)
bool = do
    pos <- MP.getOffset
    value <- MP.string "true" $> True <|> MP.string "false" $> False
    return $ Constant value <@> pos
    <?> "boolean"

withParenthesis :: Parser a -> Parser a
withParenthesis p = MP.char '(' *> MP.space *> p <* MP.space <* MP.char ')'