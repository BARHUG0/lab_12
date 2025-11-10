import Data.List

data StringOrNum = StringValue String | IntValue Int deriving (Show, Eq, Ord)

--Esto se realiza para claridad ya que los diccionarios
--seran representados como una lista de una lista de
--tuplas, es decir [[(String, StringOrNum)]]
type Key = String
type Dictionary = [(Key, StringOrNum)]
type ListOfDictionaries = [Dictionary]


exampleDictionaryList :: ListOfDictionaries
exampleDictionaryList = [
    [("make", StringValue "Nokia"), ("model", IntValue 216), ("color", StringValue "Black")],
    [("make", StringValue "Apple"), ("model", IntValue 2), ("color", StringValue "Silver")],
    [("make", StringValue "Huawei"), ("model", IntValue 50), ("color", StringValue "Gold")],
    [("make", StringValue "Samsung"), ("model", IntValue 7), ("color", StringValue "Blue")]
  ]
  
compareByKey :: Key -> Dictionary -> Dictionary -> Ordering
compareByKey key dict1 dict2 =
  let 
    maybeValue1 = lookup key dict1
    maybeValue2 = lookup key dict2
  in
    case (maybeValue1, maybeValue2) of
      (Just val1, Just val2) -> compare val1 val2
      (Just _, Nothing) -> LT
      (Nothing, Just _) -> GT
      (Nothing, Nothing) -> EQ


-- Funciones para imprimir los diccionarios en formato legible

formatDictionary :: Dictionary -> String
formatDictionary dict = 
  let
    formattedPairs = map (\(k, v) -> k ++ ": " ++ show v) dict
  in
    "{" ++ intercalate ", " formattedPairs ++ "}"

prettyPrintList :: ListOfDictionaries -> IO ()
prettyPrintList list = do  
  putStrLn "["
  mapM_ (\d -> putStrLn ("  " ++ formatDictionary d)) list
  putStrLn "]"
      

  
main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "Este es el estado actual de la lista de diccionarios\n"
  prettyPrintList exampleDictionaryList
  putStrLn "\n"
  putStrLn "Ingrese la clave por la cual desea ordenar la lista de diccionarios"
  inputKey <- getLine
  putStrLn "Este es el estado del diccionario ordenado"
  putStrLn "\n"
  let sorted = sortBy (compareByKey inputKey) exampleDictionaryList
  prettyPrintList sorted
  putStrLn "\n"