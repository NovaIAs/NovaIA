Claro! Aqui está um código em Haskell que implementa o algoritmo de ordenação Merge Sort. O Merge Sort é um algoritmo de ordenação eficiente que utiliza a técnica de dividir para conquistar.

```haskell
-- Função auxiliar que recebe duas listas ordenadas e as mescla em uma única lista ordenada
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Função principal que implementa o algoritmo de Merge Sort
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = [] -- Caso base: lista vazia, retorna vazio
mergeSort [x] = [x] -- Caso base: lista com um único elemento, já ordenada
mergeSort xs  = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
    half = div (length xs) 2 -- Divide a lista em duas partes
    firstHalf = take half xs -- Primeira metade da lista
    secondHalf = drop half xs -- Segunda metade da lista

-- Exemplo de uso
main :: IO ()
main = do
  let lista = [9, 7, 5, 1, 3, 8, 6, 2, 4] -- Lista de exemplo
  putStrLn "Lista original:"
  print lista
  putStrLn "Lista ordenada:"
  print (mergeSort lista)
```

Explicação do código:

1. A função `merge` é uma função auxiliar que recebe duas listas ordenadas e as mescla em uma única lista ordenada. Ela utiliza a técnica de comparação entre os elementos das listas para realizar a ordenação.

2. A função `mergeSort` é a função principal que implementa o algoritmo de Merge Sort. Ela utiliza a técnica de dividir a lista em duas partes, ordenar recursivamente cada parte e, em seguida, mesclar as partes ordenadas usando a função `merge`.

3. A função `main` é a função de entrada do programa. Nesse exemplo, ela cria uma lista de exemplo e chama a função `mergeSort` para ordenar a lista. Em seguida, imprime a lista original e a lista ordenada.

Esse código em Haskell implementa o algoritmo de ordenação Merge Sort de forma eficiente e é capaz de ordenar listas de qualquer tamanho.