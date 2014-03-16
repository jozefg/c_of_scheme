module Parser where
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>), many)
import AST

spaces1 :: Parser ()
spaces1 = space *> spaces

parseNum :: Parser Int
parseNum = read <$> (negInt <|> posInt)
  where negInt = (:) <$> char '-' <*> many1 digit
        posInt = many1 digit

parseIdent :: Parser String
parseIdent = do
  ident <- (:) <$> letter <*> many (alphaNum <|> oneOf "-*&^%$#_?=!")
  if ident `elem` ["lambda", "if", "set!"]
    then fail $ "Reserved word " ++ ident ++ " is improperly used as identifier"
    else return ident

parseSym :: Parser String
parseSym = char '\'' >> (:) <$> letter <*> many (alphaNum <|> oneOf "-*&^%$#_?=!")

parseLit :: Parser (SExp UserPrim)
parseLit = Lit <$> (fmap SSym parseIdent <|> fmap SInt parseNum)

parseLam :: Parser (SExp UserPrim)
parseLam = do
  char '(' *> spaces
  _ <- string "lambda"
  spaces
  vars <- char '(' *> spaces *>
          fmap SVar parseIdent `sepEndBy` spaces1
          <* char ')'
  spaces
  exps <- many1 parseSExp
  spaces *> char ')' 
  return $ Lam vars exps

parseSet :: Parser (SExp UserPrim)
parseSet = do
  char '(' *> spaces
  string "set!"
  var <- SVar <$> parseIdent <* spaces1
  exp <- parseSExp
  spaces *> char ')'
  return $ Set var exp

parseIf :: Parser (SExp UserPrim)
parseIf = do
  char '(' *> spaces
  string "if" *> spaces1
  test  <- parseSExp <* spaces
  true  <- parseSExp <* spaces
  false <- parseSExp <* spaces
  spaces <* char ')'
  return $ If test true false

parseApp :: Parser (SExp UserPrim)
parseApp = do
  fun <- parseSExp
  spaces1
  exps <- parseSExp `sepBy` spaces
  return $ App fun exps

parseVar :: Parser (SExp UserPrim)
parseVar = Var . SVar <$> parseIdent

parseSExp :: Parser (SExp UserPrim)
parseSExp = parseLit
            <|> try parseLam
            <|> try parseSet
            <|> try parseIf
            <|> parseApp
            <|> parseVar

parseSDec :: Parser (SDec UserPrim)
parseSDec = do
  spaces *> char '(' *> spaces
  string "define" *> spaces
  var <- SVar <$> parseIdent <* spaces1
  exp <- parseSExp
  spaces *> char ')'
  return $ Def var exp


parseFile :: String -> IO (Either ParseError [SDec UserPrim])
parseFile = parseFromFile (many1 parseSDec)
