module Lambda.Calculus.Parser
    ( ParserError(..)
    , parseExpr
    , renderParserError
    ) where

import Data.Void (Void)
import Lambda.Calculus.AST (Expr(..))
import Lambda.Calculus.Combinators (expandCombinators)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, parse, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

newtype ParserError = ParserError (MP.ParseErrorBundle String Void)

renderParserError :: ParserError -> String
renderParserError (ParserError bundle) = errorBundlePretty bundle

type Parser = Parsec Void String

parseExpr :: String -> Either ParserError Expr
parseExpr input =
    case parse (sc *> expression <* eof) "<input>" input of
        Left err -> Left (ParserError err)
        Right e  -> Right (expandCombinators e)

expression :: Parser Expr
expression = lambdaExpr <|> application

lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- symbol "\\" <|> symbol "λ"
    vars <- some identifier
    _ <- symbol "." <|> symbol "->"
    body <- expression
    pure $ foldr Lam body vars

application :: Parser Expr
application = do
    atoms <- some atom
    pure $ foldl1 App atoms

atom :: Parser Expr
atom = numeral <|> variable <|> parenthesised

variable :: Parser Expr
variable = Var <$> identifier

numeral :: Parser Expr
numeral = church <$> lexeme L.decimal

parenthesised :: Parser Expr
parenthesised = between (symbol "(") (symbol ")") expression

identifier :: Parser String
identifier = lexeme ((:) <$> startChar <*> many restChar)
  where
    startChar = letterChar
    restChar = alphaNumChar <|> char '\'' <|> char '_'

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

some :: Parser a -> Parser [a]
some = MP.some

church :: Integer -> Expr
church n = Lam "f" (Lam "x" (applyTimes (fromInteger n) (Var "x")))
  where
    applyTimes :: Int -> Expr -> Expr
    applyTimes count acc
        | count <= 0 = acc
        | otherwise  = App (Var "f") (applyTimes (count - 1) acc)
