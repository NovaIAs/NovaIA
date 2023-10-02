Claro! Aqui está um exemplo de código complexo em F# que utiliza funções de alta ordem e programação assíncrona:

```fsharp
open System
open System.Threading.Tasks

// Função que retorna um número aleatório de 0 a 9
let gerarNumeroAleatorio () =
    let rnd = new Random()
    rnd.Next(10)

// Função que calcula o quadrado de um número
let calcularQuadrado (x: int) =
    async {
        do! Async.Sleep(1000) // Delay de 1 segundo para simular operação assíncrona
        return x * x
    }

// Função que verifica se um número é par ou ímpar
let verificarParImpar (x: int) =
    async {
        do! Async.Sleep(2000) // Delay de 2 segundos para simular operação assíncrona
        return if x % 2 = 0 then "par" else "ímpar"
    }

// Função principal que gera um número aleatório, calcula o seu quadrado e verifica se é par ou ímpar
let main =
    async {
        let numeroAleatorio = gerarNumeroAleatorio ()
        let! quadrado = calcularQuadrado numeroAleatorio
        let! parImpar = verificarParImpar quadrado
        printfn "Número aleatório: %d" numeroAleatorio
        printfn "Quadrado: %d" quadrado
        printfn "É %s" parImpar
    }

// Execução assíncrona da função principal
[<EntryPoint>]
let mainAsync argv =
    Async.RunSynchronously main
    0
```

Neste código, temos a função `gerarNumeroAleatorio` que retorna um número aleatório de 0 a 9. A função `calcularQuadrado` recebe um número e retorna o seu quadrado de forma assíncrona, com um atraso de 1 segundo simulando uma operação demorada. A função `verificarParImpar` recebe um número e retorna se ele é par ou ímpar de forma assíncrona, com um atraso de 2 segundos simulando uma operação demorada.

Na função principal `main`, um número aleatório é gerado, seu quadrado é calculado e é verificado se é par ou ímpar. Os resultados são impressos no console.

Para executar o código, é necessário chamar a função `mainAsync` no ponto de entrada do programa.