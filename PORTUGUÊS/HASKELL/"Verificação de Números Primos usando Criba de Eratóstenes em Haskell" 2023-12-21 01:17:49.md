Claro! Aqui está um código em Haskell que realiza a verificação de números primos usando a Criba de Eratóstenes. Essa técnica consiste em encontrar todos os números primos em um intervalo pré-definido.

```haskell
module Main where

criba :: Int -> [Int] -> [Int]
criba n xs = sieve xs
  where
    sieve [] = []
    sieve (p:xs)
      | p*p > n   = p:xs
      | otherwise = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = do
  putStrLn "Digite o limite superior para encontrar os números primos:"
  input <- getLine
  let limite = read input :: Int
      primos = criba limite [2..limite]
  putStrLn ("Números primos até " ++ show limite ++ ":")
  mapM_ print primos
```

Explicação do código:

1. A função `criba` recebe um número `n` e uma lista de números `xs`.
2. A função `sieve` é uma função auxiliar que realiza a verificação dos números primos.
3. No `sieve`, primeiro verificamos se a lista está vazia. Se estiver, retornamos uma lista vazia.
4. Se a lista não estiver vazia, pegamos o primeiro elemento `p` da lista.
5. Verificamos se `p` ao quadrado é maior do que `n`. Se for, retornamos a lista atual de números primos.
6. Caso contrário, adicionamos `p` à lista de primos e chamamos recursivamente a função `sieve`, filtrando os múltiplos de `p` da lista `xs`.
7. A função `main` realiza a interação com o usuário.
8. Pede-se ao usuário que digite o limite superior para encontrar os números primos.
9. É feita a leitura do input utilizando `getLine` e é convertido para um número inteiro.
10. A função `criba` é chamada, passando o limite e uma lista de números de 2 até o limite.
11. É exibida a mensagem com o limite.
12. Utilizando a função `mapM_`, cada número primo é exibido em uma nova linha.

Espero que isso atenda às suas expectativas! Se precisar de mais alguma coisa, estou aqui para ajudar.