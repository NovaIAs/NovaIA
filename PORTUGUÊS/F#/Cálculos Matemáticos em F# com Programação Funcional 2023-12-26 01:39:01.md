Claro! Aqui está um código complexo em F# que utiliza programação funcional para calcular o fatorial de um número:

```fsharp
open System

let rec fatorial n =
    match n with
    | 0 -> 1
    | _ -> n * fatorial (n - 1)

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec somatorio n =
    match n with
    | 0 -> 0
    | _ -> n + somatorio (n - 1)

let rec isPrimo n =
    let rec isDivisor x =
        match x with
        | 1 -> true
        | _ -> if n % x = 0 then false else isDivisor (x - 1)
    isDivisor (n - 1)

let rec imprimeNumeros n =
    match n with
    | 0 -> printfn "Acabou!"
    | _ -> 
        if isPrimo n then
            printfn "%d é primo" n
            imprimeNumeros (n - 1)
        else
            printfn "%d não é primo" n
            imprimeNumeros (n - 1)

[<EntryPoint>]
let main args =
    printfn "Digite um número:"
    let numero = Console.ReadLine() |> int
    printfn "O fatorial de %d é %d" numero (fatorial numero)
    printfn "O %dº número da sequência de Fibonacci é %d" numero (fibonacci numero)
    printfn "A soma dos números de 1 a %d é %d" numero (somatorio numero)
    imprimeNumeros numero
    0
```

Este código em F# possui várias funções que realizam diferentes cálculos matemáticos. A função `fatorial` calcula o fatorial de um número utilizando recursão. A função `fibonacci` retorna o n-ésimo número da sequência de Fibonacci. A função `somatorio` calcula a soma de todos os números de 1 a n. A função `isPrimo` verifica se um número é primo ou não. E, por fim, a função `imprimeNumeros` imprime todos os números de n até 1, indicando se cada número é primo ou não.

No ponto de entrada `main`, o código solicita ao usuário para digitar um número. Em seguida, imprime o fatorial desse número, o n-ésimo número da sequência de Fibonacci, a soma dos números de 1 a n e todos os números de n até 1, indicando se cada um deles é primo ou não.

Espero que este código atenda às suas expectativas!