Claro! Aqui está um exemplo de um código complexo em Haskell, com alguns recursos avançados e uma explicação detalhada:

```haskell
module Main where

-- Função auxiliar que recebe um número inteiro n e retorna uma lista com os n primeiros números da sequência de Fibonacci
fibonacci :: Int -> [Int]
fibonacci n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Função recursiva que recebe um número inteiro n e retorna a soma dos n primeiros termos da sequência de Fibonacci
somaFibonacci :: Int -> Int
somaFibonacci n = sum (fibonacci n)

-- Função principal que lê um número inteiro do usuário e imprime a soma dos n primeiros termos da sequência de Fibonacci
main :: IO ()
main = do
  putStrLn "Digite um número inteiro:"
  input <- getLine
  let n = read input :: Int
  let soma = somaFibonacci n
  putStrLn ("A soma dos primeiros " ++ show n ++ " termos da sequência de Fibonacci é: " ++ show soma)
```

Explicação:

1. A função `fibonacci` recebe um número inteiro `n` e retorna uma lista com os `n` primeiros números da sequência de Fibonacci. A sequência de Fibonacci começa com os números 0 e 1, e cada termo subsequente é a soma dos dois termos anteriores. A função `zipWith` é utilizada para criar a lista recursivamente, somando os termos anteriores.

2. A função `somaFibonacci` recebe um número inteiro `n` e retorna a soma dos `n` primeiros termos da sequência de Fibonacci. Ela utiliza a função `fibonacci` para obter a lista de termos e, em seguida, utiliza a função `sum` para calcular a soma.

3. A função `main` é a função principal do programa. Ela solicita ao usuário que digite um número inteiro, lê o número digitado e o converte para o tipo `Int` utilizando a função `read`. Em seguida, utiliza as funções `somaFibonacci` e `show` para calcular a soma e exibi-la na tela.

Esse código em Haskell utiliza conceitos como funções, listas, recursão e entrada/saída (IO). Ele permite ao usuário obter a soma dos primeiros `n` termos da sequência de Fibonacci de forma interativa.