Claro! Aqui está um código em Haskell que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo de ordenação eficiente que divide a lista em sub-listas menores, ordena cada sub-lista e, em seguida, combina as sub-listas ordenadas para obter a lista final ordenada.

```haskell
-- Função auxiliar que combina duas listas ordenadas em uma única lista ordenada
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Função principal que implementa o Merge Sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where
        mid = length xs `div` 2
        left = take mid xs
        right = drop mid xs
```

A função `merge` é uma função auxiliar que recebe duas listas ordenadas e as combina em uma única lista ordenada. Ela usa recursão para percorrer as duas listas, comparando os elementos e adicionando-os na ordem correta na lista resultante.

A função `mergeSort` é a função principal que implementa o algoritmo do Merge Sort. Ela usa recursão para dividir a lista em sub-listas menores até que cada sub-lista contenha apenas um elemento. Em seguida, usa a função `merge` para combinar as sub-listas ordenadas e obter a lista final ordenada.

No código, a função `mergeSort` é chamada com a lista que deseja ser ordenada. Por exemplo, se chamarmos `mergeSort [4, 2, 7, 1, 5]`, o código irá ordenar a lista e retornar `[1, 2, 4, 5, 7]`.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, não hesite em pedir.