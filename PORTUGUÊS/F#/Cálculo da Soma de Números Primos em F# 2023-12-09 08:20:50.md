Certamente! Aqui está um exemplo de código em F# que calcula a soma dos números primos menores que um determinado valor:

```fsharp
open System

// Função para verificar se um número é primo
let isPrime (n: int) : bool =
    if n < 2 then false
    else
        let limit = int (sqrt (float n))
        let rec checkDivisors (i: int) : bool =
            if i > limit then true
            else if n % i = 0 then false
            else checkDivisors (i + 1)
        checkDivisors 2

// Função para calcular a soma dos números primos menores que um valor
let sumPrimes (limit: int) : int =
    let rec loop (n: int) (acc: int) : int =
        if n <= 2 then acc
        else
            let newAcc = if isPrime n then acc + n else acc
            loop (n - 1) newAcc
    loop (limit - 1) 0

// Função principal para entrada de dados e chamada da função sumPrimes
[<EntryPoint>]
let main args =
    printfn "Digite um número limite para calcular a soma dos números primos menores que ele:"
    let limit = Console.ReadLine() |> int
    let result = sumPrimes limit
    printfn "A soma dos números primos menores que %d é %d." limit result
    0 // Retorno padrão

```

Explicação do código:

1. A função `isPrime` recebe um número `n` e verifica se ele é primo ou não. Caso o número seja menor que 2, retorna `false`. Caso contrário, itera de 2 até a raiz quadrada de `n` e verifica se há algum divisor. Se encontrar um divisor, retorna `false`, caso contrário, retorna `true`.

2. A função `sumPrimes` recebe um limite e calcula a soma de todos os números primos menores que esse limite. Ela utiliza uma função recursiva chamada `loop`. O parâmetro `n` é decrementado a cada iteração, e se `n` for menor ou igual a 2, retorna o acumulador `acc`. Caso contrário, verifica se `n` é primo utilizando a função `isPrime` e, se for, adiciona `n` ao acumulador e chama recursivamente a função `loop` com `n - 1` e o novo valor do acumulador. 

3. A função principal `main` solicita ao usuário que digite um número limite para calcular a soma dos números primos menores que esse valor. O número é lido a partir da entrada padrão e convertido para inteiro. Em seguida, chama a função `sumPrimes` com o valor lido e exibe o resultado na tela.

Espero que esse código atenda às suas expectativas!