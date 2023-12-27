Claro! Aqui está um exemplo de um código complexo em F# que realiza o cálculo do número de Fibonacci:

```fsharp
open System

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let calculateFibonacciSequence limit =
    let mutable sequence = []
    for i in 0 .. limit do
        let fib = fibonacci i
        sequence <- sequence @ [fib]
    sequence

let printFibonacciSequence limit =
    let sequence = calculateFibonacciSequence limit
    printfn "Sequência de Fibonacci para os primeiros %d números:" limit
    for i in 0 .. limit do
        printfn "%d: %d" i (sequence.[i])

[<EntryPoint>]
let main argv =
    printfn "Digite um número limite para calcular a sequência de Fibonacci:"
    let limit = Console.ReadLine() |> int
    printFibonacciSequence limit
    0
```

Neste código, começamos definindo uma função `fibonacci` que retorna o n-ésimo número de Fibonacci. Em seguida, temos a função `calculateFibonacciSequence`, que recebe um limite como parâmetro e calcula uma sequência de Fibonacci até esse limite. 

A função `printFibonacciSequence` imprime a sequência calculada na tela, utilizando um loop for para percorrer os números até o limite e mostrar cada número da sequência.

No ponto de entrada `main`, solicitamos ao usuário um número limite e chamamos a função `printFibonacciSequence` para exibir a sequência de Fibonacci correspondente.

Espero que este exemplo atenda às suas expectativas!