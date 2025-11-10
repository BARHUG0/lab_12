type Matrix = [[Int]]

exampleMatrix :: Matrix
exampleMatrix =
  [ [1, 2, 3, 1],
    [4, 5, 6, 0],
    [7, 8, 9, -1]
  ]

transposeMatrix :: Matrix -> Matrix
transposeMatrix [] = []
transposeMatrix ([] : _) = []
transposeMatrix matrix =
  let firstColumn = map head matrix
      restOfMatrix = map tail matrix
   in firstColumn : transposeMatrix restOfMatrix


-- Funciones para imprimir matrices
prettyPrintMatrix :: Matrix -> IO ()
prettyPrintMatrix matrix = do  
  putStrLn "["
  mapM_ (putStrLn . ("  " ++) . show) matrix
  putStrLn "]"

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "Original Matrix: \n"
  prettyPrintMatrix exampleMatrix
  putStrLn "\n"
  putStrLn "Transposed Matrix: \n"
  let transposed = transposeMatrix exampleMatrix
  prettyPrintMatrix transposed
  putStrLn "\n"
