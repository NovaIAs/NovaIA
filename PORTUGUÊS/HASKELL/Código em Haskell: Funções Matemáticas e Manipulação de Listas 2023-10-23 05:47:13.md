Claro! Aqui está um exemplo de código complexo em Haskell:

```haskell
module Main where

import Data.List

-- Função para calcular a média de uma lista de números
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

-- Função para calcular o desvio padrão de uma lista de números
desvioPadrao :: [Double] -> Double
desvioPadrao xs = sqrt $ media $ map (\x -> (x - media xs) ^ 2) xs

-- Função para calcular o fatorial de um número
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

-- Função para gerar uma lista com todos os números primos até um determinado limite
primosAte :: Integer -> [Integer]
primosAte n = filter isPrime [2..n]
  where isPrime x = all (\y -> x `mod` y /= 0) [2..isqrt x]
        isqrt = floor . sqrt . fromIntegral

-- Função para verificar se uma lista está ordenada de forma crescente
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada xs = and $ zipWith (<=) xs (tail xs)

-- Função para encontrar o maior elemento de uma lista
maiorElemento :: Ord a => [a] -> a
maiorElemento = foldr1 max

-- Função principal
main :: IO ()
main = do
  let lista = [1, 3, 5, 2, 4]
  putStrLn "Lista original:"
  print lista
  putStrLn "Média da lista:"
  print $ media lista
  putStrLn "Desvio padrão da lista:"
  print $ desvioPadrao lista
  putStrLn "Fatorial de 5:"
  print $ fatorial 5
  putStrLn "Números primos até 10:"
  print $ primosAte 10
  putStrLn "A lista está ordenada?"
  print $ estaOrdenada lista
  putStrLn "Maior elemento da lista:"
  print $ maiorElemento lista
```

Este código em Haskell possui várias funções diferentes:

1. `media` calcula a média de uma lista de números.
2. `desvioPadrao` calcula o desvio padrão de uma lista de números.
3. `fatorial` calcula o fatorial de um número.
4. `primosAte` gera uma lista com todos os números primos até um determinado limite.
5. `estaOrdenada` verifica se uma lista está ordenada de forma crescente.
6. `maiorElemento` encontra o maior elemento de uma lista.

No `main`, a função principal, há um exemplo de uso de todas essas funções. Uma lista de números é definida e, em seguida, são impressos na tela os resultados das operações realizadas com essa lista: média, desvio padrão, fatorial de 5, números primos até 10, verificação se a lista está ordenada e o maior elemento da lista.

Espero que este exemplo atenda às suas expectativas!