Claro! Aqui está um exemplo de código em Haskell que implementa o algoritmo de ordenação merge sort:

```haskell
-- Função auxiliar que divide a lista em duas partes
split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)

-- Função auxiliar que mescla duas listas ordenadas
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Função principal que implementa o merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (ys, zs) = split xs
               in merge (mergeSort ys) (mergeSort zs)

-- Exemplo de uso
main :: IO ()
main = do
  let lista = [5, 2, 10, 1, 8]
  putStrLn "Lista original:"
  print lista
  let listaOrdenada = mergeSort lista
  putStrLn "Lista ordenada:"
  print listaOrdenada
```

Explicação do código:

1. A função `split` é uma função auxiliar que divide uma lista em duas partes iguais, utilizando recursão. Ela é responsável por dividir a lista ao meio.
2. A função `merge` é uma função auxiliar que mescla duas listas ordenadas em uma única lista ordenada, utilizando recursão e comparação entre os elementos das listas.
3. A função `mergeSort` é a função principal que implementa o algoritmo de ordenação merge sort. Ela utiliza as funções `split` e `merge` para dividir a lista original em partes menores, ordenar cada parte e, por fim, mesclar as partes ordenadas em uma única lista ordenada.
4. No exemplo de uso (`main`), é criada uma lista de exemplo `[5, 2, 10, 1, 8]` e é impressa a lista original e a lista ordenada utilizando a função `putStrLn` e `print`.

Espero que esse código em Haskell atenda às suas necessidades!