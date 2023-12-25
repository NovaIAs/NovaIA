Claro! Aqui está um código em Haskell que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo de ordenação recursivo que divide a lista em duas partes, ordena cada parte individualmente e, em seguida, mescla as partes ordenadas para obter a lista final ordenada.

```haskell
-- Função auxiliar para mesclar duas listas ordenadas
mesclar :: Ord a => [a] -> [a] -> [a]
mesclar [] ys = ys
mesclar xs [] = xs
mesclar (x:xs) (y:ys) | x <= y    = x : mesclar xs (y:ys)
                      | otherwise = y : mesclar (x:xs) ys

-- Função principal do Merge Sort
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []             -- Caso base: uma lista vazia já está ordenada
mergeSort [x] = [x]            -- Caso base: uma lista com um único elemento já está ordenada
mergeSort xs  = mesclar (mergeSort primeiraMetade) (mergeSort segundaMetade)
  where
    -- Divide a lista em duas partes
    meio          = length xs `div` 2
    primeiraMetade = take meio xs
    segundaMetade  = drop meio xs
```

Neste código, temos a função `mesclar`, que recebe duas listas ordenadas e retorna a lista resultante da mescla dessas duas listas, mantendo a ordenação. Ela utiliza a recursão para comparar o primeiro elemento de cada lista e, em seguida, concatenar o menor elemento com a mescla das duas listas restantes.

A função principal `mergeSort` é responsável por implementar o algoritmo de Merge Sort. Ela possui três casos base: quando a lista está vazia, quando a lista possui apenas um elemento e quando a lista precisa ser dividida e ordenada recursivamente. A função utiliza a função `mesclar` para combinar as listas ordenadas das duas partes da lista original e, assim, obter a lista final ordenada.

Esse código é uma implementação simples do Merge Sort em Haskell e pode ser utilizado para ordenar listas de qualquer tipo que seja uma instância da classe `Ord`.