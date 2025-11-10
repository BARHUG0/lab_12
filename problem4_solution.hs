exampleItemsList :: [String]
exampleItemsList = ["rojo", "verde", "azul", "amarillo", "gris", "blanco", "negro"]

exampleRemoveItemsList :: [String]
exampleRemoveItemsList = ["amarillo", "cafe", "blanco"]

shouldRemoveItem :: [String] -> String -> Bool
shouldRemoveItem itemsToRemove item =
  notElem item itemsToRemove

removeItems :: [String] -> [String] -> [String]
removeItems initialList elementsToRemove =
  filter (shouldRemoveItem elementsToRemove) initialList

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "Lista inicial de elementos \n"
  print exampleItemsList
  putStrLn "\n"
  putStrLn "Lista de elementos a remover \n"
  print exampleRemoveItemsList
  putStrLn "\n"
  putStrLn "Lista final de elementos \n"
  let finalList = removeItems exampleItemsList exampleRemoveItemsList
  print finalList
  putStrLn "\n"
