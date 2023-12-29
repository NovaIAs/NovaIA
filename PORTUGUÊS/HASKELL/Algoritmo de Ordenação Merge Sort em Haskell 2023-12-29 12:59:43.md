Claro! Aqui está um código em Haskell que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo eficiente que divide a lista em duas metades, ordena cada metade recursivamente e, em seguida, mescla as duas metades ordenadas em uma única lista ordenada.

```haskell
module MergeSort where

-- Função principal que recebe uma lista e retorna a lista ordenada
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = [] -- Caso base: lista vazia
mergeSort [x] = [x] -- Caso base: lista com apenas um elemento
mergeSort xs = merge (mergeSort left) (mergeSort right) -- Caso recursivo: divide a lista em duas partes e mescla

    where
        -- Divide a lista em duas partes aproximadamente iguais
        (left, right) = splitAt (length xs `div` 2) xs

        -- Mescla duas listas ordenadas em uma única lista ordenada
        merge :: Ord a => [a] -> [a] -> [a]
        merge [] ys = ys -- Caso base: primeira lista vazia
        merge xs [] = xs -- Caso base: segunda lista vazia
        merge (x:xs) (y:ys)
            | x <= y = x : merge xs (y:ys) -- Adiciona o menor elemento da primeira lista e recursivamente mescla o restante
            | otherwise = y : merge (x:xs) ys -- Adiciona o menor elemento da segunda lista e recursivamente mescla o restante
```

Explicação do código:
- A função `mergeSort` é a função principal que recebe uma lista e retorna a lista ordenada.
- Os casos base são quando a lista está vazia ou quando possui apenas um elemento, pois nesses casos ela já está ordenada.
- No caso recursivo, dividimos a lista em duas partes aproximadamente iguais usando a função `splitAt` e chamamos recursivamente o `mergeSort` para ordenar cada parte.
- Em seguida, chamamos a função `merge` para mesclar as duas partes ordenadas em uma única lista ordenada.
- A função `merge` tem como casos base quando uma das listas está vazia, pois nesse caso a mescla é simplesmente a outra lista.
- Caso contrário, comparamos o primeiro elemento de cada lista e adicionamos o menor elemento na lista de saída, chamando recursivamente a função para mesclar o restante das listas.

Esse código implementa um algoritmo de ordenação eficiente e pode ser usado em diversas situações onde seja necessário ordenar uma lista de elementos.