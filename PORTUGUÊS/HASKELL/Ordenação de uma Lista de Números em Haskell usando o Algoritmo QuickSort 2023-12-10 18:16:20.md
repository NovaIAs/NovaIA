Claro! Aqui está um exemplo de um código em Haskell que realiza a ordenação de uma lista de números usando o algoritmo QuickSort:

```haskell
-- Função principal para ordenar uma lista usando o algoritmo QuickSort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []          -- Caso base: lista vazia
quicksort (x:xs) =         -- Caso recursivo
    let smallerSorted = quicksort [a | a <- xs, a <= x]   -- Ordena os elementos menores ou iguais a x
        biggerSorted = quicksort [a | a <- xs, a > x]     -- Ordena os elementos maiores que x
    in smallerSorted ++ [x] ++ biggerSorted               -- Concatena as três partes para formar a lista ordenada

-- Função principal para testar a ordenação
main :: IO ()
main = do
    putStrLn "Digite uma lista de números separados por espaço:"
    input <- getLine
    let numbers = map read (words input) :: [Int]    -- Converte a string de entrada em uma lista de números
        sortedList = quicksort numbers                -- Chama a função quicksort para ordenar a lista
    putStrLn ("Lista ordenada: " ++ show sortedList)  -- Exibe a lista ordenada
```

Neste código, a função `quicksort` é responsável por realizar a ordenação. Ela recebe como entrada uma lista de números (`[a]`) e retorna a lista ordenada. O algoritmo QuickSort é implementado de forma recursiva.

A função `main` é a função principal do programa. Ela solicita ao usuário uma lista de números, converte a entrada em uma lista de inteiros e chama a função `quicksort` para ordenar a lista. Por fim, exibe a lista ordenada na tela.