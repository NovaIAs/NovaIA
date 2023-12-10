Claro! Aqui está um exemplo de código em Haskell que implementa o algoritmo de ordenação Bubble Sort de forma recursiva:

```haskell
module BubbleSort where

-- Função auxiliar que troca dois elementos em uma lista
troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x:y:xs)
  | x > y     = y : troca (x:xs)
  | otherwise = x : troca (y:xs)

-- Função principal que realiza o Bubble Sort recursivamente
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = let sorted = troca xs
                in  if sorted == xs
                      then xs
                      else bubbleSort sorted

-- Função para imprimir o resultado da ordenação
imprimirOrdenacao :: Show a => [a] -> IO ()
imprimirOrdenacao xs = putStrLn $ "Lista ordenada: " ++ show (bubbleSort xs)

-- Função main para executar o programa
main :: IO ()
main = do
  let lista = [4, 8, 2, 1, 5, 9, 3, 7, 6]
  putStrLn $ "Lista original: " ++ show lista
  imprimirOrdenacao lista
```

Explicação do código:

1. O código começa com a definição do módulo `BubbleSort`.

2. A função `troca` é uma função auxiliar que recebe uma lista e realiza a troca de dois elementos consecutivos quando necessário, garantindo que o maior elemento seja movido para o final da lista.

3. A função `bubbleSort` é a função principal que implementa o algoritmo do Bubble Sort de forma recursiva. Ela recebe uma lista e realiza repetidas chamadas da função `troca` até que a lista esteja completamente ordenada.

4. A função `imprimirOrdenacao` é responsável por receber uma lista ordenada e imprimir na tela o resultado.

5. A função `main` é a função principal do programa. Ela define uma lista de exemplo, imprime a lista original na tela, chama a função `imprimirOrdenacao` passando a lista original como argumento e, finalmente, executa o programa.

Espero que este exemplo te ajude a entender o algoritmo de ordenação Bubble Sort em Haskell!