module ParserExpresion where


import Text.Read (readMaybe)

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
evalExp (Add e1 e2) = (+) <$> evalInt e1 <*> evalInt e2 >>= return . (/= 0)
evalExp (Mul e1 e2) = (*) <$> evalInt e1 <*> evalInt e2 >>= return . (/= 0)
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

main :: IO ()
main = do
  let expr = And (Gt (Add (Val 10) (Mul (Val 2) (Val 3))) (Val 10)) (Not (Or (Eq (Val 5) (Val 6)) (Le (Val 7) (Val 8))))
  putStrLn $ "La expresión es: " ++ show (evalExp expr)