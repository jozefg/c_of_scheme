module Parser where
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)

parseNum :: Parser Int
parseNum = read <$> (negInt <|> posInt)
  where negInt = (:) <$> char '-' <*> many1 digit
        posInt = many1 digit

parseIdent :: Parser String
parseIdent = (:) <$> letter <*> many (alphaNum <|> oneOf "-*&^%$#_")

parseSym :: Parser String
parseSym = char '\'' *> parseIdent
