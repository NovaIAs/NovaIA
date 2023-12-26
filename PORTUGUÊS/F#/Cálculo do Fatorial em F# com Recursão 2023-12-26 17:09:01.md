Claro! Aqui está um código complexo em F# que utiliza recursão para calcular o fatorial de um número:

```fsharp
module Fatorial

open System

let rec fatorial n =
    if n < 0 then
        invalidArg "n" "O número não pode ser negativo."
    else if n = 0 then
        1
    else
        n * fatorial (n - 1)

[<EntryPoint>]
let main argv =
    printfn "Digite um número para calcular o fatorial:"
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, n -> 
        printfn "O fatorial de %d é %d." n (fatorial n)
    | _ -> 
        printfn "Entrada inválida. Por favor, digite um número inteiro válido."
    0
```

A função `fatorial` é definida como uma função recursiva que recebe um número `n` e retorna o fatorial desse número. O fatorial de um número é calculado multiplicando o número por todos os números inteiros positivos menores que ele. A função utiliza uma condicional para tratar os casos especiais em que `n` é negativo ou igual a zero.

Na função `main`, o programa solicita ao usuário que digite um número para calcular o fatorial. A entrada do usuário é lida como uma string e convertida para um número inteiro utilizando o `Int32.TryParse`. Em seguida, é feito um casamento de padrão para verificar se a conversão foi bem-sucedida. Se sim, o programa imprime o resultado do cálculo do fatorial. Caso contrário, é exibida uma mensagem de erro.

Espero que este código atenda às suas expectativas!