Claro! Aqui está um código em Haskell que implementa o algoritmo de ordenação QuickSort de forma recursiva:

```haskell
-- Função principal do QuickSort
quickSort :: Ord a => [a] -> [a]
quickSort [] = [] -- Caso base: lista vazia
quickSort (x:xs) =
  let menor = quickSort [y | y <- xs, y <= x] -- Parte esquerda
      maior = quickSort [y | y <- xs, y > x] -- Parte direita
  in menor ++ [x] ++ maior -- Concatenação das partes ordenadas

-- Função de teste
main :: IO ()
main = do
  let lista = [4, 6, 1, 3, 8, 2, 7, 5]
  putStrLn "Lista original: "
  print lista
  putStrLn "Lista ordenada: "
  print (quickSort lista)
```

Neste código, a função `quickSort` recebe uma lista de elementos `Ord a => [a]` e retorna a lista ordenada. No caso base, quando a lista está vazia, o resultado também é uma lista vazia.

Na recursão, o algoritmo seleciona um elemento `x` como pivô e separa a lista em duas partes: uma com elementos menores ou iguais a `x` e outra com elementos maiores que `x`. Em seguida, aplica-se o `quickSort` recursivamente em cada parte, ordenando-as.

No final, as partes ordenadas são concatenadas com o pivô no meio, formando a lista ordenada.

Na função `main`, é criada uma lista de exemplo `[4, 6, 1, 3, 8, 2, 7, 5]` e é impressa a lista original e a lista ordenada utilizando o `putStrLn` e `print`.

Espero que este código atenda às suas necessidades!