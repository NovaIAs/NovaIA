Claro! Aqui está um exemplo de um código complexo em F# que envolve cálculos matemáticos, manipulação de listas e recursividade:

```fsharp
open System

// Função para calcular o fatorial de um número
let rec fatorial n =
    if n = 0 then
        1
    else
        n * fatorial (n - 1)

// Função para verificar se um número é primo
let isPrimo n =
    let limite = int64(sqrt(float n))
    seq { 2L .. limite }
    |> Seq.exists (fun x -> n % int x = 0)

// Função para encontrar o maior número primo que é menor que o fatorial de um número
let maiorPrimoMenorQueFatorial n =
    let f = fatorial n
    let rec encontraPrimoMenorQueFatorial x =
        if isPrimo x && x < f then
            x
        else
            encontraPrimoMenorQueFatorial (x - 1)
    encontraPrimoMenorQueFatorial (f - 1)

// Função principal
[<EntryPoint>]
let main argv =
    printfn "Digite um número inteiro:"
    let numero = Console.ReadLine() |> int
    let result = maiorPrimoMenorQueFatorial numero
    printfn "O maior número primo menor que o fatorial de %d é %d." numero result
    0
```

Neste código em F#, temos as seguintes funções:

- `fatorial`: calcula o fatorial de um número de forma recursiva.
- `isPrimo`: verifica se um número é primo, utilizando uma sequência de números de 2 até a raiz quadrada do número.
- `maiorPrimoMenorQueFatorial`: encontra o maior número primo que é menor que o fatorial de um número. Utiliza as funções anteriores para realizar a verificação.
- `main`: função principal que solicita ao usuário um número inteiro, chama a função `maiorPrimoMenorQueFatorial` e imprime o resultado na tela.

Esse código recebe um número inteiro digitado pelo usuário e retorna o maior número primo que é menor que o fatorial desse número.