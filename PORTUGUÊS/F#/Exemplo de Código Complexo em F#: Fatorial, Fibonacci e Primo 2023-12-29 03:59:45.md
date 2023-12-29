Claro! Aqui está um exemplo de um código complexo em F# que utiliza conceitos avançados de programação funcional:

```fsharp
open System

// Função para calcular o fatorial de um número
let rec fatorial n =
    if n <= 1 then
        1
    else
        n * fatorial (n - 1)

// Função para calcular a sequência de Fibonacci até um determinado número
let rec fibonacci n =
    match n with
    | 0 -> [0]
    | 1 -> [0; 1]
    | _ -> let rec fibHelper n a b =
                if n = 0 then
                    []
                else
                    a :: fibHelper (n - 1) b (a + b)
           fibHelper (n - 2) 0 1 |> List.append [1]

// Função para verificar se um número é primo
let isPrimo n =
    let rec isPrimoHelper i =
        if i * i > n then
            true
        else if n % i = 0 then
            false
        else
            isPrimoHelper (i + 1)
    if n < 2 then
        false
    else
        isPrimoHelper 2

// Função principal
[<EntryPoint>]
let main argv =
    printfn "Bem-vindo ao programa complexo em F#!"

    printfn "Digite um número para calcular seu fatorial:"
    let inputFatorial = Console.ReadLine()
    let fatorialNumber = int inputFatorial
    let resultadoFatorial = fatorial fatorialNumber
    printfn "O fatorial de %d é %d" fatorialNumber resultadoFatorial

    printfn "Digite um número para calcular a sequência de Fibonacci:"
    let inputFibonacci = Console.ReadLine()
    let fibonacciNumber = int inputFibonacci
    let resultadoFibonacci = fibonacci fibonacciNumber
    printfn "A sequência de Fibonacci até %d é [%s]" fibonacciNumber (String.Join("; ", resultadoFibonacci))

    printfn "Digite um número para verificar se é primo:"
    let inputPrimo = Console.ReadLine()
    let primoNumber = int inputPrimo
    let resultadoPrimo = isPrimo primoNumber
    printfn "%d é primo? %b" primoNumber resultadoPrimo

    0 // Retorna 0 para indicar que o programa foi executado com sucesso
```

Este código em F# oferece três funcionalidades principais:

1. Calcula o fatorial de um número fornecido pelo usuário.
2. Gera a sequência de Fibonacci até um número fornecido pelo usuário.
3. Verifica se um número fornecido pelo usuário é primo.

Ao executar o programa, ele solicitará ao usuário para inserir os dados necessários para cada funcionalidade e, em seguida, exibirá os resultados. Cada funcionalidade é implementada como uma função separada, permitindo uma melhor organização e reutilização do código.

Espero que este exemplo atenda às suas expectativas!