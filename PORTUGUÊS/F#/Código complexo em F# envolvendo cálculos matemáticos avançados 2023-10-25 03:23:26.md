Claro! Aqui está um exemplo de um código complexo em F# que envolve cálculos matemáticos avançados:

```fsharp
open System

// Define uma função para calcular o fatorial de um número
let rec fatorial n =
    if n <= 1 then
        1
    else
        n * fatorial (n - 1)

// Define uma função para calcular o coeficiente binomial
let coeficienteBinomial n k =
    fatorial n / (fatorial k * fatorial (n - k))

// Define uma função para calcular a sequência de Fibonacci
let rec fibonacci n =
    if n <= 1 then
        n
    else
        fibonacci (n - 1) + fibonacci (n - 2)

// Define uma função que retorna o menor elemento de uma lista
let rec menorElemento lista =
    match lista with
    | [] -> failwith "A lista está vazia!"
    | [x] -> x
    | x::xs -> min x (menorElemento xs)

// Define uma função que retorna a soma dos elementos de uma lista
let rec somaElementos lista =
    match lista with
    | [] -> 0
    | x::xs -> x + somaElementos xs

// Define uma função principal que realiza diversos cálculos
let principal () =
    printfn "Digite um número inteiro n:"
    let n = Console.ReadLine() |> int

    let k = n / 2
    let coef = coeficienteBinomial n k
    printfn "O coeficiente binomial de %d escolhendo %d é: %d" n k coef

    printfn "Os primeiros %d números da sequência de Fibonacci são:" n
    for i = 0 to n - 1 do
        let fib = fibonacci i
        printf "%d " fib
    printfn ""

    printfn "Digite uma lista de números separados por espaço:"
    let numeros = Console.ReadLine() |> String.split [' ']
                                      |> List.map int

    let menor = menorElemento numeros
    printfn "O menor elemento da lista é: %d" menor

    let soma = somaElementos numeros
    printfn "A soma dos elementos da lista é: %d" soma

// Chama a função principal para iniciar o programa
principal ()
```

Neste código, temos várias funções que realizam diferentes cálculos:

1. A função `fatorial` calcula o fatorial de um número usando recursão.
2. A função `coeficienteBinomial` calcula o coeficiente binomial de dois números usando a função `fatorial`.
3. A função `fibonacci` calcula a sequência de Fibonacci usando recursão.
4. A função `menorElemento` retorna o menor elemento de uma lista usando casamento de padrões.
5. A função `somaElementos` retorna a soma dos elementos de uma lista usando casamento de padrões.
6. A função `principal` é a função principal que solicita entrada do usuário, chama as funções anteriores e exibe os resultados na tela.

Este código abrange diversos conceitos de programação funcional e pode ser usado como base para aprender mais sobre a linguagem F#.