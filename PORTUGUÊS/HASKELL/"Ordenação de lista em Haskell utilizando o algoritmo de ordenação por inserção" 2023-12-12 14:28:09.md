Claro! Aqui está um código em Haskell que realiza a ordenação de uma lista de números utilizando o algoritmo de ordenação por inserção:

```haskell
-- Função auxiliar que insere um elemento em uma lista ordenada
inserir :: Ord a => a -> [a] -> [a]
inserir x [] = [x]
inserir x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : inserir x ys

-- Função principal que ordena a lista utilizando o algoritmo de ordenação por inserção
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = inserir x (ordenar xs)

-- Função principal que recebe uma lista de números e imprime a lista ordenada
main :: IO ()
main = do
    putStrLn "Digite uma lista de números: "
    input <- getLine
    let lista = read input :: [Int]
        listaOrdenada = ordenar lista
    putStrLn "Lista ordenada: "
    print listaOrdenada
```

Neste código, temos a função `inserir` que recebe um elemento `x` e uma lista ordenada `ys` e retorna uma nova lista ordenada após inserir `x` no local correto. A função `ordenar` é a função principal que utiliza a função `inserir` para ordenar a lista de entrada.

A função `main` é a função principal que interage com o usuário. Ela solicita que o usuário digite uma lista de números, lê a entrada do usuário, converte para uma lista de números, ordena essa lista utilizando a função `ordenar` e, por fim, imprime a lista ordenada na tela.

Espero que este código atenda às suas expectativas!