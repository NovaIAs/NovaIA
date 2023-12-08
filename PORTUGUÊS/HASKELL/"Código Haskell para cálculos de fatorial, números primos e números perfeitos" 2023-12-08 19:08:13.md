Claro! Aqui está um exemplo de código em Haskell, elaborado especialmente para você:

```haskell
-- Função para calcular o fatorial de um número
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n-1)

-- Função que verifica se um número é primo
ehPrimo :: Integer -> Bool
ehPrimo n
  | n < 2 = False
  | otherwise = null [x | x <- [2..floor(sqrt (fromIntegral n))], n `mod` x == 0]

-- Função que verifica se um número é perfeito
ehPerfeito :: Integer -> Bool
ehPerfeito n = n == sum [x | x <- [1..n-1], n `mod` x == 0]

-- Função principal
main :: IO ()
main = do
  putStrLn "Digite um número inteiro:"
  nStr <- getLine
  let n = read nStr :: Integer
  putStrLn $ "O fatorial de " ++ show n ++ " é: " ++ show (fatorial n)
  putStrLn $ "O número " ++ show n ++ (if ehPrimo n then " é primo." else " não é primo.")
  putStrLn $ "O número " ++ show n ++ (if ehPerfeito n then " é perfeito." else " não é perfeito.")
```

Neste código, desenvolvido em Haskell, temos a definição de três funções:

1. `fatorial`: que recebe um número inteiro (`Integer`) e retorna o fatorial desse número. Utiliza uma função recursiva com um caso base para quando o número é 0, e recursivamente calcula o fatorial do número anterior.

2. `ehPrimo`: que recebe um número inteiro (`Integer`) e retorna um valor booleano indicando se o número é primo ou não. Utiliza uma compreensão de lista para gerar uma lista de divisores do número dado, e verifica se essa lista é vazia.

3. `ehPerfeito`: que recebe um número inteiro (`Integer`) e retorna um valor booleano indicando se o número é perfeito ou não. Utiliza uma compreensão de lista para gerar uma lista de divisores próprios do número dado (excluindo o próprio número), e verifica se a soma desses divisores é igual ao próprio número.

A função principal `main` é responsável por interagir com o usuário, solicitando um número inteiro, e exibindo as informações sobre o fatorial, se o número é primo e se o número é perfeito.

Espero que este código complexo em Haskell atenda às suas expectativas!