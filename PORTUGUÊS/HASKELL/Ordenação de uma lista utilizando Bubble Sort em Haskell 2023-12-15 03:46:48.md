Claro! Aqui está um exemplo de código em Haskell que realiza a ordenação de uma lista de números usando o algoritmo Bubble Sort:

```haskell
-- Função auxiliar que compara dois elementos e determina se estão em ordem crescente
emOrdem :: Ord a => a -> a -> Bool
emOrdem a b = a <= b

-- Função que realiza a troca de posição de dois elementos em uma lista
trocar :: [a] -> Int -> [a]
trocar [] _ = []
trocar xs i
  | i < 0 || i >= length xs = xs
  | i == length xs - 1 = xs
  | otherwise = take i xs ++ [xs !! (i + 1)] ++ [xs !! i] ++ drop (i + 2) xs

-- Função principal que realiza o Bubble Sort
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSortAux xs (length xs)

-- Função auxiliar que realiza a iteração do Bubble Sort
bubbleSortAux :: Ord a => [a] -> Int -> [a]
bubbleSortAux xs 0 = xs
bubbleSortAux xs n = bubbleSortAux (passoBubble xs) (n - 1)

-- Função auxiliar que realiza uma iteração do Bubble Sort
passoBubble :: Ord a => [a] -> [a]
passoBubble [] = []
passoBubble [x] = [x]
passoBubble (x:y:zs)
  | emOrdem x y = x : passoBubble (y:zs)
  | otherwise = y : passoBubble (trocar (x:zs) 0)

-- Exemplo de uso
main :: IO ()
main = do
  let lista = [7, 2, 5, 1, 9, 4]
  let listaOrdenada = bubbleSort lista
  putStrLn $ "Lista original: " ++ show lista
  putStrLn $ "Lista ordenada: " ++ show listaOrdenada
```

Neste código, temos a definição de algumas funções auxiliares. A função `emOrdem` compara dois elementos e retorna `True` se eles estiverem em ordem crescente. A função `trocar` recebe uma lista e um índice e realiza a troca de posição entre o elemento no índice `i` e o próximo elemento. 

A função principal `bubbleSort` recebe uma lista de elementos e realiza o Bubble Sort, chamando a função auxiliar `bubbleSortAux`. Essa função auxiliar realiza as iterações do Bubble Sort, até que todas as comparações tenham sido feitas.

A função `passoBubble` realiza uma iteração do Bubble Sort, verificando se cada par de elementos adjacentes está em ordem crescente. Se não estiverem, a função `trocar` é chamada para trocar as posições dos elementos.

No exemplo de uso, a lista `[7, 2, 5, 1, 9, 4]` é ordenada usando o Bubble Sort e o resultado é exibido na saída padrão.