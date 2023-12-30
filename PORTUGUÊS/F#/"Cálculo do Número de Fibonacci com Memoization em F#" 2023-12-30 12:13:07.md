Claro! Aqui está um código complexo em F# que calcula o número de Fibonacci para um determinado valor, usando memoization para melhorar o desempenho:

```fsharp
open System.Collections.Generic

let rec FibonacciMemo (memo: Dictionary<int, bigint>) (n: int) : bigint =
    if n <= 1 then bigint(n)
    elif memo.ContainsKey(n) then memo.[n]
    else
        let result = FibonacciMemo memo (n - 1) + FibonacciMemo memo (n - 2)
        memo.Add(n, result)
        result

let Fibonacci (n: int) : bigint =
    let memo = Dictionary<int, bigint>()
    FibonacciMemo memo n

[<EntryPoint>]
let main argv =
    printfn "Digite o valor para calcular o número de Fibonacci:"
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, n -> printfn "O %dº número de Fibonacci é: %A" n (Fibonacci n)
    | _ -> printfn "Entrada inválida"
    0
```

Neste código, estamos utilizando a técnica de memoization para evitar recálculo de valores já encontrados. A função `FibonacciMemo` recebe um dicionário `memo` que armazena os valores já calculados. Ela verifica se o valor já está presente no dicionário antes de realizar o cálculo.

A função `Fibonacci` é a função principal que cria um novo dicionário `memo` e chama a função `FibonacciMemo` para obter o número de Fibonacci desejado.

No `main`, pedimos ao usuário para digitar o valor para calcular o número de Fibonacci e, em seguida, chamamos a função `Fibonacci` passando o valor digitado como argumento.

Espero que isso atenda às suas expectativas de um código complexo e diferenciado em F#!