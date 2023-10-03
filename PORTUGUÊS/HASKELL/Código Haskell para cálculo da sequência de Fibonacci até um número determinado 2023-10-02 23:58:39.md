Claro! Aqui está um exemplo de código em Haskell que cria uma função para calcular a sequência de Fibonacci até um determinado número:

```haskell
module Fibonacci where

fibonacci :: Int -> [Int]
fibonacci n = takeWhile (<= n) fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

Neste código, criamos um módulo chamado "Fibonacci". 

A função `fibonacci` recebe um número inteiro `n` como parâmetro e retorna uma lista de inteiros contendo os números da sequência de Fibonacci até `n`. 

A sequência de Fibonacci começa com os números 0 e 1, e a partir do terceiro número, cada número é a soma dos dois números anteriores. 

A implementação utiliza uma função auxiliar chamada `fibs`, que é uma lista infinita de todos os números da sequência de Fibonacci. 

Através da função `takeWhile`, filtramos apenas os números menores ou iguais a `n` para evitar uma lista infinita. 

O resultado é uma lista com os números da sequência de Fibonacci até o número fornecido. 

Por exemplo, ao chamar `fibonacci 10`, o resultado será `[0,1,1,2,3,5,8]`, pois são os números da sequência menores ou iguais a 10.