import Text.Read (readMaybe)

calculateNthPower :: Int -> Int -> Int
calculateNthPower power base = base ^ power

calculateNthPowerForList :: Int -> [Int] -> [Int]
calculateNthPowerForList n ints = map (calculateNthPower n) ints

exampleInts :: [Int] 
exampleInts = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "Esta es la lista de números"
  putStrLn "\n"
  print exampleInts
  putStrLn "\n"
  putStrLn "¿Cuál es la potencia a la que quieres elevar cada número?"
  nStr <- getLine
  case readMaybe nStr :: Maybe Int of
    Just n -> do
     let result = calculateNthPowerForList n exampleInts
     putStrLn "El resultado de elevar cada número a la potencia indicada es:"
     putStrLn "\n"
     
     print result
     putStrLn "\n"
    Nothing -> do
     putStrLn "Por favor, introduce un número válido."
     putStrLn "\n"

  

    
