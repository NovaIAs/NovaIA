Claro! Aqui está um código complexo em Haskell que realiza uma série de operações matemáticas e manipulações de listas:

```haskell
module Complexo where

import Data.List (sort)

-- Definindo um tipo de dado complexo
data Complexo = Complexo Double Double

-- Função para somar dois números complexos
somaComplexos :: Complexo -> Complexo -> Complexo
somaComplexos (Complexo a b) (Complexo c d) = Complexo (a + c) (b + d)

-- Função para subtrair dois números complexos
subtraiComplexos :: Complexo -> Complexo -> Complexo
subtraiComplexos (Complexo a b) (Complexo c d) = Complexo (a - c) (b - d)

-- Função para multiplicar dois números complexos
multiplicaComplexos :: Complexo -> Complexo -> Complexo
multiplicaComplexos (Complexo a b) (Complexo c d) = Complexo ((a * c) - (b * d)) ((a * d) + (b * c))

-- Função para calcular o módulo de um número complexo
moduloComplexo :: Complexo -> Double
moduloComplexo (Complexo a b) = sqrt ((a * a) + (b * b))

-- Função para calcular a média de uma lista de números complexos
mediaComplexos :: [Complexo] -> Complexo
mediaComplexos xs = let soma = foldl somaComplexos (Complexo 0 0) xs
                        n = fromIntegral (length xs)
                    in Complexo ((realPart soma) / n) ((imaginaryPart soma) / n)

-- Função para ordenar uma lista de números complexos pelo módulo
ordenarPorModulo :: [Complexo] -> [Complexo]
ordenarPorModulo xs = sort xs (\a b -> compare (moduloComplexo a) (moduloComplexo b))

-- Exemplo de uso das funções

complexo1 = Complexo 3 4
complexo2 = Complexo (-2) 5

soma = somaComplexos complexo1 complexo2
subtracao = subtraiComplexos complexo1 complexo2
multiplicacao = multiplicaComplexos complexo1 complexo2
modulo = moduloComplexo complexo1

listaComplexos = [complexo1, complexo2, Complexo 1 2, Complexo (-3) 1]

media = mediaComplexos listaComplexos
listaOrdenada = ordenarPorModulo listaComplexos
```
Neste código, definimos um tipo de dado `Complexo` que representa um número complexo com partes real e imaginária. Em seguida, implementamos funções para realizar operações matemáticas básicas com números complexos, como soma, subtração e multiplicação. Também definimos uma função para calcular o módulo de um número complexo.

Além disso, incluímos uma função `mediaComplexos` que recebe uma lista de números complexos e calcula a média deles, retornando um número complexo médio. Também implementamos uma função `ordenarPorModulo` que ordena uma lista de números complexos com base no valor do seu módulo.

No exemplo de uso do código, criamos dois números complexos `complexo1` e `complexo2` e realizamos diversas operações com eles, como soma, subtração, multiplicação e cálculo do módulo. Também criamos uma lista de números complexos `listaComplexos` e utilizamos as funções `mediaComplexos` e `ordenarPorModulo` com essa lista.

Espero que este código atenda às suas expectativas!