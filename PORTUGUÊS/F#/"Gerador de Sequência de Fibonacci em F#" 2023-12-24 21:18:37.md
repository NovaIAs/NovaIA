Claro! Aqui está um exemplo de um código complexo em F# que envolve manipulação de listas e recursão:

```fsharp
open System

let rec gerarSequenciaFibonacci n =
    match n with
    | 0 -> []
    | 1 -> [0]
    | 2 -> [0; 1]
    | _ ->
        let rec loop i fibs =
            if i = n then fibs
            else
                let nextFib = List.sum (List.take 2 (List.rev fibs))
                loop (i + 1) (nextFib :: fibs)
        loop 3 [1; 0]

let imprimirSequenciaFibonacci n =
    let fibs = gerarSequenciaFibonacci n
    printfn "Sequência de Fibonacci com %d números:" n
    fibs |> List.iter (printf "%d ")

[<EntryPoint>]
let main argv =
    printfn "Digite um número para gerar a sequência de Fibonacci:"
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, n when n > 0 -> imprimirSequenciaFibonacci n
    | _ -> printfn "Número inválido!"
    0
```

Neste código, a função `gerarSequenciaFibonacci` utiliza recursão para gerar uma sequência de Fibonacci com base no número fornecido. A função `imprimirSequenciaFibonacci` recebe o número gerado e imprime a sequência na tela. O programa principal captura a entrada do usuário, converte-a para um número inteiro e chama a função `imprimirSequenciaFibonacci` se o número for válido.

A sequência de Fibonacci é uma sequência de números em que cada número é a soma dos dois anteriores: 0, 1, 1, 2, 3, 5, 8, 13, etc.