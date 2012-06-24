module RegExp.Parser(RegExp(..), parseRegExp) where

import Control.Applicative ((<$>),(*>),(<*),pure)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)

data RegExp a =
    Literal a
    | Or (RegExp a) (RegExp a)
    | Then (RegExp a) (RegExp a)
    | Star (RegExp a)
    deriving (Eq, Show)

group :: Parser (RegExp Char)
group = char '(' *> regexp <* char ')'

regexp :: Parser (RegExp Char)
regexp = chainl1 repetition (char '|' *> pure Or)

repetition :: Parser (RegExp Char)
repetition = chainl1 piece (pure Then)

piece :: Parser (RegExp Char)
piece = do
    v <- atom
    option v (char '*'  >>  (return . Star $ v))

atom :: Parser (RegExp Char)
atom = Literal <$> alphaNum <|> group

parseRegExp :: String -> Either ParseError (RegExp Char)
parseRegExp = parse regexp ""
