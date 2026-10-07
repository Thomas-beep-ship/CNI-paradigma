module Parser where

import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String)}

-- instances

-- change result
instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Nothing -> Nothing
        Just (x, rest) -> Just (f x, rest)

--pure
instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)

    Parser pf <*> Parser px = Parser $ \input -> case pf input of
        Nothing -> Nothing
        Just (f, rest) -> case px rest of
            Nothing -> Nothing
            Just (x, rest') -> Just (f x, rest')
            
-- >>=
instance Monad Parser where
    Parser p >>= f = Parser $ \input -> case p input of
        Nothing -> Nothing
        Just (x, rest) -> runParser (f x) rest

-- empty
instance Alternative Parser where
    empty = Parser (const Nothing)

    Parser p <|> Parser q = Parser $ \input -> case p input of
        Nothing -> q input
        result -> result

-- building blocks

-- one character statifies
satisfy :: (Char -> Bool) -> Parser Char
satisfy ok = Parser $ \input -> case input of
    c : cs | ok c -> Just (c, cs)
    _             -> Nothing

-- Exactly this char
char :: Char -> Parser Char
char c = satisfy (== c)

-- Only succeeds at the end of the input.
eof :: Parser ()
eof = Parser $ \input -> case input of
  [] -> Just ((), [])
  _  -> Nothing

-- One or more p, separated by sep:  p (sep p)*
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- Run a parser on the whole string: all input must be used.
parse :: Parser a -> String -> Maybe a
parse p input = fst <$> runParser (p <* eof) input