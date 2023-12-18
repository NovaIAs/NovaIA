Claro! Aqui está um código em Haskell que utiliza conceitos avançados da linguagem para realizar uma tarefa complexa. Vou explicar cada parte do código para facilitar o entendimento:

```haskell
module Main where

import Data.List

-- Função para calcular o fatorial de um número
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- Função para verificar se um número é primo
ehPrimo :: Integer -> Bool
ehPrimo n
    | n <= 1 = False
    | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]
    where isqrt = floor . sqrt . fromIntegral

-- Função para gerar uma lista de números primos utilizando uma expressão lambda
gerarPrimos :: Integer -> [Integer]
gerarPrimos n = filter (\x -> ehPrimo x) [2..n]

-- Função para calcular a soma dos fatoriais de uma lista de números
somaFatoriais :: [Integer] -> Integer
somaFatoriais xs = sum [fatorial x | x <- xs]

-- Função principal
main :: IO ()
main = do
    putStrLn "Digite um número inteiro positivo:"
    input <- getLine
    let n = read input :: Integer
    let primos = gerarPrimos n
    let soma = somaFatoriais primos
    putStrLn ("A soma dos fatoriais dos números primos até " ++ show n ++ " é: " ++ show soma)
```

Neste código, começamos importando o módulo `Data.List`, que nos dá acesso a funções úteis para trabalhar com listas. Em seguida, definimos a função `fatorial`, que calcula o fatorial de um número. A função `ehPrimo` verifica se um número é primo utilizando uma lista de compreensão e a função `isqrt`, que calcula a raiz quadrada de um número e transforma o resultado em um número inteiro utilizando as funções `floor` e `fromIntegral`.

A função `gerarPrimos` utiliza a função `filter` juntamente com uma expressão lambda para gerar uma lista de números primos até um certo limite. A função `somaFatoriais` recebe uma lista de números e calcula a soma dos fatoriais de cada elemento.

Na função principal `main`, utilizamos a função `putStrLn` para exibir uma mensagem pedindo ao usuário para digitar um número inteiro positivo. Em seguida, utilizamos `getLine` para ler o número digitado pelo usuário e armazená-lo na variável `input`. Utilizamos `read` para converter `input` em um número inteiro e armazená-lo na variável `n`.

Utilizamos as funções `gerarPrimos` e `somaFatoriais` para calcular a soma dos fatoriais dos números primos até `n` e armazenamos o resultado na variável `soma`.

Por fim, utilizamos `putStrLn` para exibir a mensagem com o resultado, concatenando o valor de `n` e `soma` utilizando a função `show` para convertê-los em strings.

Espero que este código atenda às suas expectativas! Caso precise de mais alguma coisa, estou à disposição.