module Parser where
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import Control.Monad
import AST

parseNum :: Parser Int
parseNum = fmap read $ (:) <$> (char '-' <|> return '0') <*> many1 digit

parseIdent :: Parser String
parseIdent = (:) <$> (letter <|> syms) <*> many (alphaNum <|> syms)
  where syms = oneOf"!@#$%^&*-_+=\\/~?<>,."

parseVar :: Parser Var
parseVar = SVar <$> parseIdent

parseSym :: Parser (SExp UserPrim)
parseSym = char '\'' >> Lit . SSym <$> parseIdent

parseSInt :: Parser (SExp UserPrim)
parseSInt = Lit . SInt <$> parseNum

spaces1 :: Parser ()
spaces1 = spaces *> spaces 
spaced = (<* spaces)

paren :: Parser a -> Parser a
paren p = char '(' *> spaces *> p <* spaces <* char ')'

parseSet :: Parser (SExp UserPrim)
parseSet = paren $ do
  string "set!" *> spaces1
  var <- spaced parseVar
  Set var <$> parseExp
  
parseIf :: Parser (SExp UserPrim)
parseIf = paren $ do
  string "if" *> spaces1
  If <$> spaced parseExp <*> spaced parseExp <*> parseExp


parseApp :: Parser (SExp UserPrim)
parseApp = paren $ do
  fun <- spaced parseExp
  App fun <$> many (spaced parseExp)

parseLam :: Parser (SExp UserPrim)
parseLam = paren $ do
  string "lambda" *> spaces
  args <- paren $ many (spaced parseVar)
  Lam args <$> many1 (spaced parseExp)
  
parseExp :: Parser (SExp UserPrim)
parseExp = parseSInt
           <|> parseSym
           <|> Var <$> parseVar
           <|> try parseSet
           <|> try parseIf
           <|> try parseLam
           <|> parseApp
           <?> "Couldn't parse expression"

parseDec :: Parser (SDec UserPrim)
parseDec = paren $ do
  string "define" *> spaces1
  var <- spaced parseVar
  Def var <$> parseExp

parseFile :: String -> IO (Either ParseError [SDec UserPrim])
parseFile = parseFromFile (spaces *> many1 (spaced parseDec))
