module Parser (parse, Expr (..)) where

import Control.Applicative
import Data.Char (isAlpha)
import Data.List (intercalate)

type Error = String

data Expr = Atom String | List [Expr] | Int Int | Bool Bool deriving Eq

instance Show Expr where
    show (Atom s) = s
    show (List es) = "(" ++ intercalate " " (show <$> es) ++ ")"
    show (Int i) = show i
    show (Bool b) = show b

newtype Parser a = P {unP :: String -> (String, Either Error a)}

instance Functor Parser where
    fmap f (P st) = P $ \stream ->
        let (res, mbP) = st stream
         in (res, f <$> mbP)

instance Applicative Parser where
    pure a = P (\stream -> (stream, Right a))
    P f <*> P x = P $ \stream ->
        let (stream', mbF) = f stream
         in let (stream'', mbX) = x stream'
             in (stream'', mbF <*> mbX)

instance Alternative Parser where
    empty = P $ \stream -> (stream, Left "empty")

    (<|>) (P f) (P f') = P $ \stream -> case f stream of
        (stream', Right a) -> (stream', Right a)
        (_, Left _) -> f' stream

    many (P f) = P go
      where
        go stream = case f stream of
            (_, Left _) -> (stream, Right [])
            (stream', Right a) ->
                let (streamFin, mbAs) = go stream'
                 in (streamFin, (a :) <$> mbAs)

    some (P f) = (:) <$> P f <*> many (P f)

-- Consumes next character if predicate met
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = P $ \stream -> case stream of
    [] -> ([], Left "end of stream")
    (c : cs)
        | f c -> (cs, Right c)
        | otherwise -> (cs, Left "did not satisfy")

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = pure []
string (c : cs) = (:) <$> char c <*> string cs

spaces :: Parser ()
spaces = P $ \stream -> (dropWhile (== ' ') stream, Right ())

bool :: Parser Expr
bool = (Bool True <$ string "#t") <|> (Bool False <$ string "#f")

numeric :: Parser Expr
numeric = Int . read <$> (some $ satisfy (`elem` "0123456789"))

alpha :: Parser Expr
alpha = Atom <$> (some $ satisfy isAlpha)

special :: Parser Expr
special = Atom <$> (some $ satisfy (`elem` "+/-*"))

literal :: Parser Expr
literal = numeric <|> alpha <|> bool <|> special

list :: Parser Expr
list = char '(' *> spaces *> (List <$> many (expr <* spaces)) <* char ')'

expr :: Parser Expr
expr = list <|> literal

parse :: String -> Either Error Expr
parse s = case (unP expr) s of
    (_, Left err) -> Left err
    ("", Right expr') -> Right expr'
    (_, Right _) -> Left "stream not fully consumed"
