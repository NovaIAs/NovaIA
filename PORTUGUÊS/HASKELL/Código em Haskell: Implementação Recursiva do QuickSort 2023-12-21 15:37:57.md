Claro! Aqui está um código em Haskell que implementa o algoritmo de ordenação QuickSort de forma recursiva:

```haskell
module Main where

-- Função principal
main :: IO ()
main = do
    let listaDesordenada = [9, 5, 2, 7, 1, 8, 4, 6, 3]
    putStrLn "Lista desordenada:"
    print listaDesordenada
    let listaOrdenada = quicksort listaDesordenada
    putStrLn "Lista ordenada:"
    print listaOrdenada

-- Função de ordenação QuickSort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] -- Caso base, lista vazia
quicksort (x:xs) =
    let menores = quicksort [a | a <- xs, a <= x] -- Parte recursiva para os elementos menores
        maiores = quicksort [a | a <- xs, a > x] -- Parte recursiva para os elementos maiores
    in menores ++ [x] ++ maiores -- Concatenação dos elementos menores, pivô e elementos maiores
```

Explicação do código:

1. Começamos importando o módulo `Main`, que é necessário para executar o programa em Haskell.

2. A função principal `main` é onde a lista desordenada é definida e o algoritmo de ordenação é chamado. Primeiro, exibimos a lista desordenada e, em seguida, exibimos a lista ordenada.

3. A função `quicksort` implementa o algoritmo de ordenação QuickSort de forma recursiva. Ela recebe uma lista como parâmetro e retorna a lista ordenada. 

4. No caso base, quando a lista é vazia, retornamos uma lista vazia.

5. Caso contrário, selecionamos o primeiro elemento da lista como pivô e dividimos a lista em duas partes: uma contendo os elementos menores ou iguais ao pivô (`menores`), e outra contendo os elementos maiores que o pivô (`maiores`).

6. Chamamos a função `quicksort` recursivamente para ordenar as listas `menores` e `maiores`, e então concatenamos os elementos menores, o pivô e os elementos maiores na ordem correta.

Este código em Haskell implementa uma versão simples do algoritmo QuickSort e é capaz de ordenar qualquer lista de elementos comparáveis.