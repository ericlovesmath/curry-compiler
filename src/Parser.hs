module Parser (parse, Expr (..)) where

import Control.Applicative
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intercalate)

type Error = String

data Expr = Atom String | List [Expr] | Int Integer | Bool Bool deriving (Eq)

instance Show Expr where
    show (Atom s) = s
    show (List es) = "(" ++ intercalate " " (map show es) ++ ")"
    show (Int i) = show i
    show (Bool b) = if b then "#t" else "#f"

newtype Parser a = Parser {runParser :: String -> Either Error (String, a)}

instance Functor Parser where
    fmap f (Parser px) =
        Parser $ \input -> do
            (input', x) <- px input
            return (input', f x)

instance Applicative Parser where
    pure a = Parser (\input -> Right (input, a))

    Parser pf <*> Parser px =
        Parser $ \input -> do
            (input', f) <- pf input
            (input'', x) <- px input'
            return (input'', f x)

instance Alternative Parser where
    empty = Parser $ const (Left "empty")

    Parser p <|> Parser p' =
        Parser $ \input ->
            case p input of
                Left _ -> p' input
                result -> result

instance Monad Parser where
    Parser p >>= f =
        Parser $ \input -> do
            (input', x) <- p input
            runParser (f x) input'

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = (:) <$> p <*> many (sep *> p) <|> pure []

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond =
    Parser $ \input -> case input of
        c : cs | cond c -> Right (cs, c)
        [] -> Left "End of Stream"
        _ -> Left $ "Failed to Satisfy"

commentP :: Parser ()
commentP = () <$ (charP ';' *> many (satisfy (/= '\n')))

strip :: Parser ()
strip = () <$ many (() <$ satisfy isSpace <|> commentP)

spaces :: Parser ()
spaces = () <$ some (() <$ satisfy isSpace <|> commentP)

charP :: Char -> Parser Char
charP c = satisfy (== c)

stringP :: String -> Parser String
stringP "" = pure ""
stringP (c : cs) = (:) <$> charP c <*> stringP cs

boolP :: Parser Bool
boolP = (True <$ stringP "#t") <|> (False <$ stringP "#f")

numberP :: Parser Integer
numberP =
    read <$> (digits <|> (:) <$> charP '-' <*> digits)
  where
    digits = some (satisfy isDigit)

alphaP :: Parser String
alphaP = some $ satisfy isAlpha

specialP :: Parser Char
specialP = satisfy (`elem` "+/-*=")

literalP :: Parser Expr
literalP =
    (Int <$> numberP)
        <|> (Bool <$> boolP)
        <|> (Atom <$> alphaP)
        <|> (Atom . pure <$> specialP)

listP :: Parser Expr
listP = List <$> (bounded '(' ')' <|> bounded '[' ']' <|> bounded '{' '}')
  where
    exprs = strip *> sepBy spaces exprP <* strip
    bounded opening closing = charP opening *> exprs <* charP closing

exprP :: Parser Expr
exprP = listP <|> literalP

parse :: String -> Either Error Expr
parse input = case (runParser $ strip *> exprP <* strip) input of
    Right ("", expr) -> Right expr
    Right _ -> Left "Input not fully consumed"
    Left err -> Left err
