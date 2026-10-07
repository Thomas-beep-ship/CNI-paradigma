module RegexParser where

import Control.Applicative
import Parser
import Regex


parseRegex :: String -> Maybe Regex
parseRegex = parse regex

matchPattern :: String -> String -> Maybe Bool
matchPattern pat s = (`matches` s) <$> parseRegex pat

regex :: Parser Regex
regex = foldr1 alt <$> sepBy1 sequenceP (char '|')

sequenceP :: Parser Regex
sequenceP = foldr cat Eps <$> many repetition

repetition :: Parser Regex
repetition = do
    a <- atom
    ops <- many postfix
    pure (foldl (\r op -> op r) a ops)

postfix :: Parser (Regex -> Regex)
postfix = star <$ char '*'
        <|> plus <$ char '+'
        <|> opt <$ char '?'

atom :: Parser Regex
atom = char '(' *> regex <* char ')'
    <|> Chr anyChar <$ char '.'
    <|> lit <$> satisfy (`notElem` specials)

specials :: String
specials = "\\.*+?|()[{^$"