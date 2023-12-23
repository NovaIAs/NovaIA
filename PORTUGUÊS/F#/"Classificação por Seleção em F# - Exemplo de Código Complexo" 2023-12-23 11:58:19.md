Claro! Aqui está um exemplo de código complexo em F# que realiza uma classificação por seleção em uma lista de números:

```fsharp
open System

// Função para trocar dois elementos em uma lista
let swap (lista: 'a list) (i: int) (j: int) =
    let mutable mutableList = List.toArray lista
    let temp = mutableList.[i]
    mutableList.[i] <- mutableList.[j]
    mutableList.[j] <- temp
    List.ofArray mutableList

// Função para encontrar o índice do menor elemento em uma lista
let rec encontraIndiceMenor (lista: int list) (indice: int) (menorIndice: int) (menorValor: int) =
    match lista with
    | [] -> menorIndice
    | cabeca :: cauda ->
        if cabeca < menorValor then
            encontraIndiceMenor cauda (indice + 1) indice cabeca
        else
            encontraIndiceMenor cauda (indice + 1) menorIndice menorValor

// Função principal de classificação por seleção
let rec selecaoSort (lista: int list) (indice: int) =
    match lista with
    | [] -> []
    | _ ->
        let menorIndice = encontraIndiceMenor lista indice indice (List.head lista)
        let listaOrdenada = swap lista indice menorIndice
        List.head listaOrdenada :: selecaoSort (List.tail listaOrdenada) (indice + 1)

// Teste do algoritmo de classificação por seleção
[<EntryPoint>]
let main argv =
    let numeros = [7; 3; 9; 2; 1; 5; 8; 4; 6]
    let numerosOrdenados = selecaoSort numeros 0

    printfn "Números ordenados: %A" numerosOrdenados

    0 // Retorna um código de saída para o sistema
```

Este código implementa o algoritmo de classificação por seleção em F#. A função `swap` é usada para trocar dois elementos em uma lista. A função `encontraIndiceMenor` encontra o índice do menor elemento em uma lista, dado um índice inicial e o menor valor encontrado até o momento. A função principal `selecaoSort` usa as funções anteriores para classificar a lista de entrada.

No teste do algoritmo, uma lista de números é criada e passada para a função `selecaoSort` com um índice inicial de 0. Os números ordenados são então impressos no console.

Espero que este código atenda às suas expectativas!