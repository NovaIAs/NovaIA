Claro, aqui está um exemplo de um código em Haskell que implementa uma calculadora simples com as operações de soma, subtração, multiplicação e divisão. 

```haskell
-- Este é um exemplo de uma calculadora simples em Haskell

-- Função principal
main :: IO ()
main = do
    putStrLn "Bem-vindo à calculadora Haskell!"
    putStrLn "Escolha uma operação:"
    putStrLn "1 - Soma"
    putStrLn "2 - Subtração"
    putStrLn "3 - Multiplicação"
    putStrLn "4 - Divisão"
    escolha <- getLine
    calcular escolha

-- Função que recebe a escolha do usuário e realiza a operação correspondente
calcular :: String -> IO ()
calcular "1" = soma
calcular "2" = subtracao
calcular "3" = multiplicacao
calcular "4" = divisao
calcular _   = putStrLn "Opção inválida!"

-- Funções para cada operação
soma :: IO ()
soma = do
    putStrLn "Digite o primeiro número:"
    numero1 <- getLine
    putStrLn "Digite o segundo número:"
    numero2 <- getLine
    let resultado = read numero1 + read numero2
    putStrLn ("O resultado da soma é: " ++ show resultado)

subtracao :: IO ()
subtracao = do
    putStrLn "Digite o primeiro número:"
    numero1 <- getLine
    putStrLn "Digite o segundo número:"
    numero2 <- getLine
    let resultado = read numero1 - read numero2
    putStrLn ("O resultado da subtração é: " ++ show resultado)

multiplicacao :: IO ()
multiplicacao = do
    putStrLn "Digite o primeiro número:"
    numero1 <- getLine
    putStrLn "Digite o segundo número:"
    numero2 <- getLine
    let resultado = read numero1 * read numero2
    putStrLn ("O resultado da multiplicação é: " ++ show resultado)

divisao :: IO ()
divisao = do
    putStrLn "Digite o dividendo:"
    dividendo <- getLine
    putStrLn "Digite o divisor:"
    divisor <- getLine
    if read divisor /= 0 then do 
        let resultado = read dividendo / read divisor
        putStrLn ("O resultado da divisão é: " ++ show resultado)
    else
        putStrLn "Não é possível dividir por zero!"
```

Nesse código, temos uma calculadora que permite ao usuário escolher uma operação (soma, subtração, multiplicação ou divisão) e em seguida digitar os valores necessários para realizar a operação. Depois, o resultado é calculado e exibido na tela. 

A função `main` é a função principal, que apresenta as opções de operação para o usuário e captura a escolha. Em seguida, chama a função `calcular` passando a escolha como argumento.

A função `calcular` é responsável por realizar a operação correspondente com base na escolha do usuário. Cada operação possui uma função específica, como `soma`, `subtracao`, `multiplicacao` e `divisao`. Cada uma dessas funções solicita os valores necessários para realizar a operação e mostra o resultado na tela.

Esse código é apenas um exemplo de uma calculadora simples em Haskell, mas pode ser facilmente expandido adicionando mais operações ou funcionalidades.