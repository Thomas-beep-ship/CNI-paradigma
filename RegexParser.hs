module RegexParser where

import Control.Applicative
import Parser
import Regex
import Data.Char ( isDigit ) 


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

bounds :: Parser (Regex -> Regex)
bounds = do
  _ <- char '{'
  n <- number
  m <- (char ',' *> optional number) <|> pure (Just n)
  _ <- char '}'
  case m of
    Just m' | m' < n -> empty
    _                -> pure (repeatRange n m)

number :: Parser Int
number = read <$> some (satisfy isDigit)

postfix :: Parser (Regex -> Regex)
postfix =  star <$ char '*'
       <|> plus <$ char '+'
       <|> opt  <$ char '?'
       <|> bounds     

escape :: Parser CharSet
escape = do
  c <- satisfy (const True)
  pure $ case c of
    'd' -> digitSet
    'w' -> wordSet
    's' -> spaceSet
    'D' -> complementSet digitSet
    'W' -> complementSet wordSet
    'S' -> complementSet spaceSet
    'n' -> singleton '\n'
    't' -> singleton '\t'
    _   -> singleton c

charClass :: Parser CharSet
charClass = do
  _     <- char '['
  neg   <- (True <$ char '^') <|> pure False
  first <- ((\c -> [(c, c)]) <$> char ']') <|> pure []
  rest  <- many classItem
  _     <- char ']'
  pure (CharSet neg (first ++ concat rest))

classItem :: Parser [(Char, Char)]
classItem = predefined <|> rangeOrChar
  where
    predefined = char '\\' *> (setRanges <$> (    digitSet <$ char 'd'
                                              <|> wordSet  <$ char 'w'
                                              <|> spaceSet <$ char 's'))
    rangeOrChar = do
      a  <- classChar
      mb <- optional (char '-' *> classChar)
      case mb of
        Nothing         -> pure [(a, a)]
        Just b
          | a <= b      -> pure [(a, b)]
          | otherwise   -> empty
    classChar = (char '\\' *> satisfy (const True)) <|> satisfy (/= ']')                          

atom :: Parser Regex
atom =  char '(' *> regex <* char ')'
    <|> Chr <$> charClass                     
    <|> Chr anyChar <$ char '.'
    <|> Chr <$> (char '\\' *> escape)
    <|> lit <$> satisfy (`notElem` specials)
specials :: String
specials = "\\.*+?|()[{^$"