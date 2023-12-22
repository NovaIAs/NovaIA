Claro, vou gerar um código complexo em Haskell para você. Aqui está o código:

```haskell
module Principal where

import Data.List (sort)

-- | Definição do tipo de dado 'Pessoa'
data Pessoa = Pessoa { nome :: String
                     , idade :: Int
                     , salario :: Double
                     } deriving (Show)

-- | Função para ordenar uma lista de pessoas pelo salário em ordem decrescente
ordenarPorSalario :: [Pessoa] -> [Pessoa]
ordenarPorSalario = reverse . sortOn salario

-- | Função para calcular a média de idade de uma lista de pessoas
calcularMediaIdade :: [Pessoa] -> Double
calcularMediaIdade pessoas = fromIntegral (sum idades) / fromIntegral (length pessoas)
  where idades = map idade pessoas

-- | Função para filtrar as pessoas com salário acima de um valor dado
filtrarPorSalario :: Double -> [Pessoa] -> [Pessoa]
filtrarPorSalario valor = filter (\p -> salario p > valor)

-- | Função para calcular a soma dos salários das pessoas com idade entre dois valores dados
calcularSomaSalarios :: Int -> Int -> [Pessoa] -> Double
calcularSomaSalarios minIdade maxIdade pessoas = sum salarios
  where salarios = map salario (filter (\p -> idade p >= minIdade && idade p <= maxIdade) pessoas)

-- | Função principal que será executada quando o programa for executado
main :: IO ()
main = do
  let pessoas = [ Pessoa "Joao" 25 2000.0
                , Pessoa "Maria" 30 2500.0
                , Pessoa "Pedro" 40 3000.0
                , Pessoa "Ana" 35 2200.0
                , Pessoa "Lucas" 28 1800.0
                ]
  putStrLn "Pessoas:"
  print pessoas

  let pessoasOrdenadas = ordenarPorSalario pessoas
  putStrLn "\nPessoas ordenadas pelo salário (decrescente):"
  print pessoasOrdenadas

  let mediaIdade = calcularMediaIdade pessoas
  putStrLn $ "\nMédia de idade das pessoas: " ++ show mediaIdade

  let salarioMin = 2200.0
  let pessoasFiltradas = filtrarPorSalario salarioMin pessoas
  putStrLn $ "\nPessoas com salário acima de " ++ show salarioMin ++ ":"
  print pessoasFiltradas

  let idadeMin = 25
  let idadeMax = 35
  let somaSalarios = calcularSomaSalarios idadeMin idadeMax pessoas
  putStrLn $ "\nSoma dos salários das pessoas com idade entre " ++ show idadeMin ++ " e " ++ show idadeMax ++ ":"
  print somaSalarios
```

Este código é um exemplo de um programa em Haskell que manipula uma lista de pessoas. Ele contém as seguintes funções:

1. `ordenarPorSalario`: recebe uma lista de pessoas e retorna a lista ordenada pelo salário em ordem decrescente.
2. `calcularMediaIdade`: recebe uma lista de pessoas e calcula a média de idade delas.
3. `filtrarPorSalario`: recebe um valor de salário e uma lista de pessoas, e retorna uma nova lista contendo apenas as pessoas com salário acima desse valor.
4. `calcularSomaSalarios`: recebe dois valores de idade e uma lista de pessoas, e retorna a soma dos salários das pessoas que têm idade entre esses valores.
5. `main`: a função principal que será executada quando o programa for executado. Neste exemplo, ela cria uma lista de pessoas, executa as funções acima e imprime os resultados na tela.

Espero que este código atenda às suas necessidades. Se você tiver alguma dúvida ou precisar de mais exemplos, fique à vontade para perguntar!