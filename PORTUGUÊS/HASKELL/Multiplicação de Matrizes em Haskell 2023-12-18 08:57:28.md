Claro! Aqui está um código em Haskell que realiza a multiplicação de matrizes:

```haskell
-- Função que realiza a multiplicação de duas matrizes
multiplicarMatrizes :: [[Int]] -> [[Int]] -> [[Int]]
multiplicarMatrizes matriz1 matriz2 =
  let
    -- Função auxiliar que realiza a multiplicação de uma linha da primeira matriz pela coluna da segunda matriz
    multiplicarLinhaColuna :: [Int] -> [[Int]] -> [Int]
    multiplicarLinhaColuna linha coluna =
      let
        -- Função auxiliar que realiza a multiplicação de um elemento da linha pelo elemento correspondente na coluna
        multiplicarElementos :: [Int] -> [Int] -> Int
        multiplicarElementos [] [] = 0
        multiplicarElementos (x:xs) (y:ys) = (x * y) + multiplicarElementos xs ys
      in
        map (\c -> multiplicarElementos linha c) (transpose coluna)
  in
    map (\l -> multiplicarLinhaColuna l matriz2) matriz1

-- Função que transpõe uma matriz
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose matriz = (map head matriz) : transpose (map tail matriz)

-- Função principal que testa a multiplicação de matrizes
main :: IO ()
main = do
  let matriz1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  let matriz2 = [[9, 8, 7], [6, 5, 4], [3, 2, 1]]
  let resultado = multiplicarMatrizes matriz1 matriz2
  putStrLn "Matriz 1:"
  print matriz1
  putStrLn "Matriz 2:"
  print matriz2
  putStrLn "Resultado da multiplicação das matrizes:"
  print resultado
```

Neste código, a função `multiplicarMatrizes` recebe duas matrizes como parâmetros e retorna o resultado da multiplicação dessas matrizes. A função `multiplicarLinhaColuna` é uma função auxiliar que realiza a multiplicação de uma linha da primeira matriz pela coluna da segunda matriz. A função `multiplicarElementos` é outra função auxiliar que realiza a multiplicação de um elemento da linha pelo elemento correspondente na coluna.

A função `transpose` é responsável por transpor uma matriz, ou seja, trocar suas linhas por colunas. Essa função é utilizada na função `multiplicarLinhaColuna` para transpor a matriz `matriz2` e realizar a multiplicação corretamente.

No `main`, são definidas duas matrizes de exemplo, `matriz1` e `matriz2`. Em seguida, a função `multiplicarMatrizes` é chamada com essas matrizes como argumentos, e o resultado é armazenado na variável `resultado`. Por fim, os valores das matrizes e o resultado da multiplicação são exibidos na tela.