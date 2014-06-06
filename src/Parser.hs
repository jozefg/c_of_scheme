module Parser where
import Control.Applicative hiding ((<|>), many)
import Control.Monad.Trans
import Control.Error
import Text.Parsec
import Text.Parsec.String
import AST
import Utils.Error

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

spaced :: Parser a -> Parser a
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
  args <- spaced . paren $ many (spaced parseVar)
  Lam args <$> many1 (spaced parseExp)
  
parseExp :: Parser (SExp UserPrim)
parseExp = Var <$> parseVar
           <|> parseSym
           <|> parseSInt
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

parseFile :: String -> IO (Compiler [SDec UserPrim])
parseFile = fmap (lift . hoistEither . intoFail) . parseFromFile (spaces *> many1 (spaced parseDec))
  where intoFail (Left e)  = Left $ Failure Parser "parseSDec" (show e)
        intoFail (Right r) = Right r
