
import Text.ParserCombinators.Parsec
import ParserExpresion (parseExp,evalExp)
import Control.Arrow (Arrow(first))
data TemplateElement = TextElement String | VariableElement String | IfElement String [TemplateElement] | LoopElement String [TemplateElement] deriving Show

-- Parser para elementos de plantilla
templateParser :: Parser [TemplateElement]
templateParser = many templateElementParser

templateElementParser :: Parser TemplateElement
templateElementParser = variableParser <|> ifParser <|> loopParser <|> textParser

variableParser :: Parser TemplateElement
variableParser = do
  char '{'
  char '{'
  variableName <- many1 (noneOf "{}")
  char '}'
  char '}'
  return (VariableElement variableName)

ifParser :: Parser TemplateElement
ifParser = do
  string "{% if "
  condition <- many1 (noneOf " %}")
  string " %}"
  elements <- templateParser
  string "{% endif %}"
  return (IfElement condition elements)

loopParser :: Parser TemplateElement
loopParser = do
  string "{% for "
  variableName <- many1 (noneOf " in ")
  string " in "
  listName <- many1 (noneOf " %}")
  string " %}"
  elements <- templateParser
  string "{% endfor %}"
  return (LoopElement variableName elements)

textParser :: Parser TemplateElement
textParser = do
  text <- many1 (noneOf "{%")
  return (TextElement text)

-- Genera el contenido final reemplazando las variables con sus valores
generateContent :: [(String, String)] -> [TemplateElement] -> String
generateContent _ [] = ""

generateContent variables (TextElement text : xs) = text ++ generateContent variables xs

generateContent variables (VariableElement name : xs) =
  case lookup name variables of ---- lookup :: Eq a => a -> [(a, b)] -> Maybe b 
  --Ex1 : lookup 2 [(1, "first")] = Nothing 
  --Ex2:lookup 2 [(1, "first"), (2, "second"), (3, "third")] = Just "second
    Just value -> value ++ generateContent variables xs
    Nothing -> generateContent variables xs

--generateContent variables (IfElement condition elements : xs) =
 -- if evaluateCondition condition variables
   -- then generateContent variables elements ++ generateContent variables xs
  --  else generateContent variables xs

-- Evalúa una condición
evaluateCondition :: String -> [(String, String)] -> Maybe Bool
evaluateCondition condition variables =
  let replacedCondition = replaceVariables condition variables
  in case parseExp replacedCondition of
    Left err -> Nothing
    Right expr -> evalExp expr
 
-- Divide una lista separada por comas en elementos individuales
splitList :: String -> [String]
splitList = words . map (\c -> if c == ',' then ' ' else c)

-- Reemplaza las variables en una cadena con sus valores
replaceVariables :: String -> [(String,String)] -> String
replaceVariables str [] = str
replaceVariables str ((var,val):dict) =
  let splList= splitSpace str 
      replacedStr = replace splList var val
  in replaceVariables replacedStr dict


splitSpace :: String -> [String]
splitSpace str = filter (not . null) (splitSpaceA str "")

splitSpaceA :: String -> String -> [String]
splitSpaceA "" "" = []
splitSpaceA "" palabraActual = [palabraActual]
splitSpaceA (c:cs) palabraActual
    | c == ' ' = palabraActual : splitSpaceA cs ""
    | otherwise = splitSpaceA cs (palabraActual ++ [c])

replace :: [String]-> String->String->String
replace [] _ _ = ""
replace (x:xs) var value
        |x == var = value ++ " " ++ replace xs var value
        |otherwise = x ++ " " ++ replace xs var value
          
    
-- Ejemplo de uso
main :: IO ()
main = do
  let template = "Hola, {{nombre}}!\n\n{% if edad > 18 %}\nEres mayor de edad.Eres menor de edad.\n{% endif %}\n\n{% for item in lista %}\n- {{item}}\n{% endfor %}"
  let variables = [("nombre", "Juan"), ("edad", "22"), ("lista", "manzana, naranja, plátano")]
  case parse templateParser "" template of
    Right parsedTemplate -> do
      let content = generateContent variables parsedTemplate
      writeFile "output.html" content
      putStrLn "Archivo generado correctamente."
    Left err -> putStrLn $ "Error al analizar la plantilla: " ++ show err
