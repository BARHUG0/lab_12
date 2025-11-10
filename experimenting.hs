data StringOrNum = StringValue String | IntValue Int deriving (Show, Eq, Ord)

--Esto se realiza para claridad ya que los diccionarios
--seran representados como una lista de una lista de
--tuplas, es decir [[(String, StringOrNum)]]
type Dictionary = [(String, StringOrNum)]
type ListOfDictionaries = [Dictionary]


--Area de experimentos/aprendizaje

getValueByKey :: String -> Dictionary -> Maybe StringOrNum
getValueByKey key dictionary = lookup key dictionary

countDictionaries :: [[(String, StringOrNum)]] -> Int
countDictionaries dictionaryList = length dictionaryList

exampleDictionaryList :: [[(String, StringOrNum)]]
exampleDictionaryList = [
    [("make", StringValue "Nokia"), ("model", IntValue 216), ("color", StringValue "Black")],
    [("make", StringValue "Apple"), ("model", IntValue 2), ("color", StringValue "Silver")],
    [("make", StringValue "Huawei"), ("model", IntValue 50), ("color", StringValue "Gold")],
    [("make", StringValue "Samsung"), ("model", IntValue 7), ("color", StringValue "Blue")]
  ]
  
exampleDictionary :: [(String, StringOrNum)]
exampleDictionary = [
    ("name", StringValue "James"),
    ("age", IntValue 12),
    ("hobbie", StringValue "guitar")
  ]
  

main :: IO ()
main = do
  print $ countDictionaries exampleDictionaryList
  print $ getValueByKey "name" exampleDictionary
