Claro! Aqui está um código complexo em Haskell que utiliza conceitos avançados da linguagem:

```haskell
module Main where

import Data.List (permutations)

-- Função para verificar se um número é primo
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (\x -> n `mod` x /= 0) [2..(floor . sqrt . fromIntegral) n]

-- Função para verificar se uma lista é uma permutação de números primos
isPrimePermutation :: [Int] -> Bool
isPrimePermutation xs = all isPrime xs && any (\ys -> xs == ys) (permutations xs)

-- Função para obter todas as permutações de números primos em uma lista de inteiros
primePermutations :: [Int] -> [[Int]]
primePermutations xs = filter isPrimePermutation (permutations xs)

-- Função para imprimir todas as permutações de números primos em uma lista de inteiros
printPrimePermutations :: [Int] -> IO ()
printPrimePermutations xs = mapM_ print (primePermutations xs)

main :: IO ()
main = do
  putStrLn "Digite uma lista de números inteiros separados por espaço:"
  input <- getLine
  let numbers = map read (words input)
  printPrimePermutations numbers
```

Explicação do código:

1. Importamos o módulo `Data.List` para usar a função `permutations`, que gera todas as permutações de uma lista.
2. Definimos a função `isPrime` para verificar se um número é primo. Ela retorna `False` se o número for igual a 1 e utiliza a função `all` e `mod` para verificar se o número é divisível por algum número entre 2 e a raiz quadrada do número.
3. Definimos a função `isPrimePermutation` que recebe uma lista de números e verifica se ela é uma permutação de números primos. Utilizamos a função `all` para verificar se todos os números da lista são primos e a função `any` para verificar se existe alguma permutação da lista que seja igual à lista original.
4. Definimos a função `primePermutations` que recebe uma lista de inteiros e retorna todas as permutações de números primos dessa lista. Utilizamos a função `filter` para filtrar apenas as permutações que são números primos.
5. Definimos a função `printPrimePermutations` que recebe uma lista de inteiros e imprime todas as permutações de números primos dessa lista. Utilizamos a função `mapM_` em conjunto com a função `print` para imprimir cada permutação em uma nova linha.
6. A função `main` é a função principal do programa. Ela solicita ao usuário que digite uma lista de números inteiros separados por espaço, lê o input do usuário usando `getLine`, converte os números para uma lista de inteiros usando `map read`, e em seguida chama a função `printPrimePermutations` para imprimir todas as permutações de números primos da lista digitada pelo usuário.