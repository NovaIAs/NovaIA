Claro! Aqui está um código complexo em F# que realiza a manipulação de uma matriz bidimensional de números inteiros, aplicando algumas operações matemáticas:

```fsharp
open System

let matriz : int[,] = Array2D.init 3 3 (fun i j -> i + j)

let somarElementos (m: int[,]) =
    let mutable soma = 0
    for i = 0 to m.GetLength(0) - 1 do
        for j = 0 to m.GetLength(1) - 1 do
            soma <- soma + m.[i, j]
    soma

let produtoDiagonalPrincipal (m: int[,]) =
    let mutable produto = 1
    for i = 0 to m.GetLength(0) - 1 do
        produto <- produto * m.[i, i]
    produto

let mediaLinha (m: int[,]) =
    let mutable media = 0.0
    for i = 0 to m.GetLength(0) - 1 do
        let mutable soma = 0
        for j = 0 to m.GetLength(1) - 1 do
            soma <- soma + m.[i, j]
        media <- media + float soma
    media / float m.GetLength(0)

let imprimirMatriz (m: int[,]) =
    for i = 0 to m.GetLength(0) - 1 do
        for j = 0 to m.GetLength(1) - 1 do
            printf "%d " m.[i, j]
        printfn ""

let main() =
    printfn "Matriz original:"
    imprimirMatriz matriz

    let soma = somarElementos matriz
    printfn "\nA soma dos elementos da matriz é: %d" soma

    let produto = produtoDiagonalPrincipal matriz
    printfn "O produto dos elementos da diagonal principal é: %d" produto

    let media = mediaLinha matriz
    printfn "A média dos elementos de cada linha é: %.2f" media

main()
```

Neste código, definimos uma matriz bidimensional `matriz` de tamanho 3x3, onde cada elemento é a soma das coordenadas da posição. O código inclui também algumas funções:

- `somarElementos` percorre todos os elementos da matriz e retorna a soma de todos eles.
- `produtoDiagonalPrincipal` percorre a diagonal principal da matriz e retorna o produto de todos os elementos.
- `mediaLinha` calcula a média dos elementos de cada linha da matriz e retorna a média total.
- `imprimirMatriz` imprime a matriz na tela.

Na função `main`, chamamos cada uma dessas funções e imprimimos os resultados na tela.