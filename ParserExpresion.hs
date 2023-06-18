module ParserExpresion(
  parseExp,
  evalExp
)
where
import Text.Parsec
    ( 
      spaces,
      string,
      digit,
      many1,
      parse,
      ParseError,
      space,
      eof,
      skipMany1,
      (<?>) )
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import Control.Applicative (liftA2, (*>),(<*), (<$>), (<*>), (<|>))

data Exp = Val Int
         | Add Exp Exp
         | Mul Exp Exp
         | Gt Exp Exp
         | Eq Exp Exp
         | Le Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         deriving (Show, Eq)

evalExp :: Exp -> Maybe Bool
evalExp (Val x) = Just $ x /= 0
evalExp (Add e1 e2) = (/= 0) <$> ((+) <$> evalInt e1 <*> evalInt e2)
evalExp (Mul e1 e2) = (/= 0) <$> ((*) <$> evalInt e1 <*> evalInt e2)
evalExp (Gt e1 e2) = (>) <$> evalInt e1 <*> evalInt e2
evalExp (Eq e1 e2) = (==) <$> evalInt e1 <*> evalInt e2
evalExp (Le e1 e2) = (<=) <$> evalInt e1 <*> evalInt e2
evalExp (And e1 e2) = (&&) <$> evalExp e1 <*> evalExp e2
evalExp (Or e1 e2) = (||) <$> evalExp e1 <*> evalExp e2
evalExp (Not e) = not <$> evalExp e

evalInt :: Exp -> Maybe Int
evalInt (Val x) = Just x
evalInt (Add e1 e2) = (+) <$> evalInt e1 <*> evalInt e2
evalInt (Mul e1 e2) = (*) <$> evalInt e1 <*> evalInt e2
evalInt _ = Nothing

-- Parser combinators
spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol :: String -> Parser String
symbol s = string s <* spaces

integer :: Parser Int
integer = do
  num <- many1 digit
  spaces
  case readMaybe num of
    Just n -> return n
    Nothing -> fail "No se pudo leer el entero"

parenthesized :: Parser a -> Parser a
parenthesized p = symbol "(" *> p <* symbol ")"

binOp :: String -> (Exp -> Exp -> Exp) -> Parser (Exp -> Exp -> Exp)
binOp s op = symbol s *> return op

expParser :: Parser Exp
expParser = buildExpressionParser table factor
  where
    table = [ [ Infix (binOp "*" Mul) AssocLeft ]
           , [ Infix (binOp "+" Add) AssocLeft ]
           , [ Infix (binOp "==" Eq) AssocNone
             , Infix (binOp "<=" Le) AssocNone
             , Infix (binOp ">" Gt) AssocNone ]
           , [ Prefix (symbol "not" *> return Not) ]
           , [ Infix (binOp "&&" And) AssocRight
             , Infix (binOp "||" Or) AssocRight ] ]

    factor = Val <$> integer
         <|> parenthesized expParser
         <?> "expresión"

parseExp :: String -> Either ParseError Exp
parseExp = parse (expParser <* eof) "Expresión"

main :: IO ()
main = do
  let input = "10 + 2 * 3 > 10 && (5 == 6 || 7 <= 8)"
  case parseExp input of
    Left err -> putStrLn $ "Error: " ++ show err
    Right expr -> putStrLn $ "La expresión es: " ++ show (evalExp expr)          
