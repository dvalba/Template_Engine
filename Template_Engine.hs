import Text.ParserCombinators.Parsec
import ParserExpresion (parseExp,evalExp)
import Control.Arrow (Arrow(first))
import Data.String (IsString)
import Text.Parsec (parse)
import Data.List (isPrefixOf, elemIndex,tails)
import Data.String (IsString)
import Language.Haskell.TH (Strict)
import Numeric.Natural
import Data.Maybe (maybeToList)
import Distribution.Simple.InstallDirs (substituteInstallDirTemplates)
import Data.Array.IArray (Ix(index))
import Data.ByteString (split)

data TemplateElement =
    TextElement String
    |VariableElement String
    |IfElement String String
    deriving (Eq,Show)

type Template = [TemplateElement]
-- Parser Template Element
parser ::String -> Template
parser  text
  |parseBVar text =
      let (var,rest) = parseVar text
      in VariableElement var : parser  rest
  |parseBIf text =
      let (condition,elements,rest) = parseIf text
      in IfElement condition elements : parser  rest
  |parseBTxt text =
      let (txt,rest) = parseTxt text
      in TextElement txt : parser  rest
  |otherwise = []

parseTxt :: String -> (String,String)
parseTxt ('{':'*':res) =
  case break (== '*') res of
    (txt,'*':'}':rest) -> (txt,rest)
    (_, _) -> error "Invalid pattern"

parseIf:: String -> (String,String,String)
parseIf('{':'%':' ':'i':'f':'-':x:res) =
  case break (== '%') res of
    (condition,'%':'}':rest) ->
      let (elements,resto) = parseEndIf x rest
      in (condition,elements,resto)
    (_,_) -> error "Invalid pattern"

parseVar :: String -> (String,String)
parseVar ('{':'{':res) =
  case break (== '}') res of
    (var,'}':'}':rest) -> (var,rest)
    (_, _) -> error "Invalid pattern"

parseEndIf::Char -> String -> (String,String)
parseEndIf x str =
  let flag = "{% endif-"++[x]++" %}"
  in case splitStr flag str of
    (elements,'{':'%':' ':'e':'n':'d':'i':'f':'-':x:' ':'%':'}':rest) -> (elements,rest)
    (_,_)->("error","error")

splitStr :: String -> String -> (String,String)
splitStr subst str =
  let
    index = findSubstrIndex subst str 0
    (element,rest) = splitAt index str
  in
    (element,rest)

findSubstrIndex :: String -> String -> Int -> Int
findSubstrIndex (x:xs) []  index = -1
findSubstrIndex [] (y:ys) index = -1
findSubstrIndex (x:xs) (y:ys) index
  | x == y =  if(subst (x:xs) (y:ys))
                then
                  index
              else
                findSubstrIndex (x:xs) ys (index+1)
  |otherwise = findSubstrIndex (x:xs) ys (index+1)

subst :: String -> String -> Bool
subst (x:xs) [] = False
subst [x] (y:ys) = x == y
subst (x:xs) (y:ys) =
  (x == y) && subst xs ys

parseBTxt :: String -> Bool
parseBTxt ('{':'*':_) = True
parseBTxt _ = False

parseBIf :: String -> Bool
parseBIf ('{':'%':' ':'i':'f':'-':_) = True
parseBIf _ = False

parseBVar :: String -> Bool
parseBVar ('{':'{':_) = True
parseBVar _ = False

-- Genera el contenido final reemplazando las variables con sus valores
generateContent :: [(String, String)] -> Template -> String
generateContent _ [] = ""

generateContent variables (TextElement text : xs) = text ++ generateContent variables xs

generateContent variables (VariableElement name : xs) =
  case lookup name variables of -- lookup :: Eq a => a -> [(a, b)] -> Maybe b --Ex1 : lookup 2 [(1, "first")] = Nothing --Ex2:lookup 2 [(1, "first"), (2, "second"), (3, "third")] = Just "second
    Just value -> value ++ generateContent variables xs
    Nothing -> generateContent variables xs

generateContent variables (IfElement condition elements : xs) =
  case evaluateCondition condition variables of
    Just value -> if value
                  then
                    let elem = parser elements
                    in generateContent variables elem ++ generateContent variables xs
                  else generateContent variables xs
    Nothing -> generateContent variables xs

-- Evalúa una condición
evaluateCondition :: String -> [(String, String)] -> Maybe Bool
evaluateCondition condition variables
  | replacedCondition == "True " = Just True
  | replacedCondition == "False " = Just False
  | otherwise =
       case parseExp replacedCondition of
        Left err -> Nothing
        Right expr -> evalExp expr
  where replacedCondition = replaceVariables condition variables
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
  let template = "{% if-1 pincha %}{* Hola los if pinchan *}{% if-2 edad > 18 %}{* Eres Mayor de Edad *}{% endif-2 %}{% endif-1 %}{*El texto pincha y las variables*}{{Var}}"
    -- \n{% for item in lista %}\n- {{item}}\n{% endfor %}"
  let
    variables = [("Var","Tambien"),("pincha","True"),("edad","19")]
    parsedTemplate = parser template
    content = generateContent variables parsedTemplate
  writeFile "output.html" content
  putStrLn "Archivo generado correctamente."


