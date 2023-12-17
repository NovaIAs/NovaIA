Claro! Aqui está um código em Haskell que implementa o algoritmo de ordenação chamado "Merge Sort" de forma recursiva. O Merge Sort é um algoritmo eficiente e de complexidade O(n log n) para ordenar uma lista.

```haskell
-- Função que divide uma lista em duas partes aproximadamente iguais
divide :: [a] -> ([a], [a])
divide [] = ([], [])
divide [x] = ([x], [])
divide (x:y:xs) = (x:ys, y:zs)
    where (ys, zs) = divide xs

-- Função que mescla duas listas ordenadas em uma única lista ordenada
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Função principal que implementa o Merge Sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
    where (ys, zs) = divide xs

-- Exemplo de uso
main :: IO ()
main = do
    let listaDesordenada = [9, 5, 2, 7, 1, 8, 3, 6, 4]
    putStrLn "Lista desordenada:"
    print listaDesordenada
    putStrLn "Lista ordenada usando Merge Sort:"
    print (mergeSort listaDesordenada)
```

Explicação do código:

- A função `divide` recebe uma lista e a divide em duas partes aproximadamente iguais, utilizando recursão. Caso a lista seja vazia, retorna duas listas vazias. Se a lista tiver apenas um elemento, retorna uma lista com esse elemento e uma lista vazia. Caso contrário, utiliza o padrão `(x:y:xs)` para dividir a lista em duas partes, sendo `x` o primeiro elemento, `y` o segundo elemento e `xs` o restante da lista. Em seguida, chama recursivamente a função `divide` passando `xs` e obtém duas listas, `ys` e `zs`, que representam as partes divididas da lista original.

- A função `merge` recebe duas listas ordenadas e as mescla em uma única lista ordenada. Se uma das listas for vazia, retorna a outra lista. Caso contrário, utiliza o padrão `(x:xs)` para obter o primeiro elemento `x` e o restante `xs` da primeira lista, e o mesmo para a segunda lista `(y:ys)`. Compara `x` e `y` e, se `x` for menor ou igual a `y`, concatena `x` com a chamada recursiva passando `xs` e a segunda lista, caso contrário concatena `y` com a chamada recursiva passando a primeira lista e `ys`.

- A função `mergeSort` é a função principal que implementa o Merge Sort. Caso a lista seja vazia, retorna uma lista vazia. Se a lista tiver apenas um elemento, retorna a própria lista. Caso contrário, utiliza o padrão `(ys, zs) = divide xs` para obter as partes divididas da lista `xs`. Em seguida, chama recursivamente a função `mergeSort` passando `ys` e `zs`, e mescla as duas listas ordenadas resultantes utilizando a função `merge`.

- Por fim, no exemplo de uso `main`, é definida uma lista desordenada `listaDesordenada`. Em seguida, a lista é impressa na tela. Depois, é chamada a função `mergeSort` passando a `listaDesordenada` como argumento, e o resultado é impresso na tela, representando a lista ordenada utilizando o Merge Sort.

Espero que esse código atenda às suas expectativas!