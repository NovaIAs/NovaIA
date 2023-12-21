Claro! Aqui está um exemplo de um código complexo em F# que utiliza programação funcional para calcular a série de Fibonacci de forma recursiva:

```fsharp
open System

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let imprimirFibonacci n =
    for i = 0 to n do
        printfn "Fibonacci(%d) = %d" i (fibonacci i)

[<EntryPoint>]
let main argv =
    printfn "Digite o número limite para calcular a série de Fibonacci:"
    let limite = Console.ReadLine() |> int
    imprimirFibonacci limite
    0
```

Explicação do código:

- A função `fibonacci` é definida como uma função recursiva. Ela recebe um número `n` como parâmetro e retorna o n-ésimo número da série de Fibonacci.
- A série de Fibonacci começa com os números 0 e 1, e cada número subsequente é a soma dos dois números anteriores.
- A função `imprimirFibonacci` recebe um número `n` como parâmetro e imprime a série de Fibonacci até o n-ésimo número.
- No bloco principal, o usuário é solicitado a digitar o número limite para calcular a série de Fibonacci.
- O número digitado é convertido para inteiro e passado como argumento para a função `imprimirFibonacci`.
- Por fim, o programa retorna 0.

Espero que esse código complexo em F# atenda às suas expectativas. Lembre-se de que você pode adaptá-lo e modificá-lo conforme necessário.